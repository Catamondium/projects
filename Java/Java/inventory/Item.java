package inventory;
import java.util.*;
import java.util.function.*;
import java.util.stream.Collectors;

/**
 * Template Item
 * <p>
 * Implements stats derivation and accumulation
 * </p>
 */
public abstract class Item implements Itemic {
    String name;
    List<Integer> data = new ArrayList<Integer>();
    int acc = 0;

    public Item(String name) {
        this.name = name;
    }

    @Override
    public String getName() {
        return name;
    }

    @Override
    public float mean() {
        return acc / data.size();
    }

    @Override
    public int max() {
        return data.stream().max(Comparator.naturalOrder()).orElse(0);
    }

    @Override
    public int min() {
        return data.stream().min(Comparator.naturalOrder()).orElse(0);
    }

    @Override
    public int median() {
        var sorted = data.stream().sorted().collect(Collectors.toList());
        return sorted.get(sorted.size() / 2);
    }

    @Override
    public int mode() {
        int m = 0;
        long count = 0;
        var freqs = data.stream().collect(Collectors.groupingBy(Function.identity(), Collectors.counting()));

        for (var e : freqs.entrySet()) {
            if (e.getValue() > count) {
                count = e.getValue();
                m = e.getKey();
            }
        }

        return m;
    }

    @Override
    public void clear() {
        data.clear();
    }

    @Override
    public void request(int n) {
        data.add(n);
        acc += n;
    }
}