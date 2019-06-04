/*
Crawls 'example.co.uk' counting occurences of 'Product not found!',
crawls are made almost concurrently, to avoid saturating tcp sockets we mutex the GET requests.
Due to the mutex, testing all 1E5 product IDs is unreasonably slow
so, instead we take a randomised sample and scale up to approximate
the number of products available.

NOTE: IDs are 0-filled 5 width decimal in this case
*/
package main

import (
	"fmt"
	"io/ioutil"
	"math/rand"
	"net/http"
	"os"
	"strings"
	"sync"
	"time"
)

func randomSet(n, max int) []int {
	if max < n {
		panic(fmt.Sprintf("Insufficient range: [0, %d) to fill %d spots", max, n))
	}

	set := make(map[int]bool, n)
	ret := make([]int, n)
	rand.Seed(time.Now().UnixNano())

	for len(set) < n {
		set[rand.Intn(max)] = true
	}

	i := 0
	for k := range set {
		ret[i] = k
		i++
	}

	return ret
}

const max = 1E5
const sampleRatio = 0.1
const sample = max * sampleRatio
const template = "https://www.example.co.uk/product_info.php?products_id=%05d"

func crawl(id int, ret chan<- string, access *sync.Mutex) {

	source := fmt.Sprintf(template, id)

	access.Lock()
	resp, err := http.Get(source)
	access.Unlock()
	if err != nil {
		ret <- fmt.Sprintf("%v", err)
		return
	}

	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		ret <- fmt.Sprintf("%v", err)
		return
	}

	sbody := string(body)
	if !strings.Contains(sbody, "Product not found!") {
		ret <- "active"
	} else {
		ret <- "inactive"
	}

	return
}

func percent(x, max int) int {
	return (100 * x) / max
}

func scale(x int, ratio float64) int {
	return int(float64(x) / ratio)
}

func main() {
	c := make(chan string, sample)
	getAccess := &sync.Mutex{}

	set := randomSet(sample, max)

	for i := 0; i < sample; i++ {
		go crawl(set[i], c, getAccess)
	}

	active := 0
	inactive := 0
	for i := 0; i < sample; i++ {
		switch b := <-c; b {
		case "active":
			active++
		case "inactive":
			inactive++
		default:
			fmt.Fprintf(os.Stderr, "%s\n\n", b)
		}
	}

	fmt.Printf("Set: %v\n", set)
	fmt.Printf("Active: %d\n", active)
	fmt.Printf("Inactive: %d\n", inactive)
	fmt.Printf("Errors: %d\n", sample-(active+inactive))

	fmt.Printf("~%02d%% activity\n", percent(active, sample))
	fmt.Printf("~%d actually active\n", scale(active, sampleRatio))
}
