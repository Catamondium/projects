console.log("BG");

var re = /.+(\.jpg|\.jpeg|\.png)$/i;
function runtab(tabs) {
	console.log("PRESSED");
	let imgtabs = tabs.filter(tab => {
		let check = new URL(tab.url);
		return re.test(check.pathname);
	});
	console.log(imgtabs);
	for(let tab of imgtabs) {
		console.log(`Found: ${tab.url}`);
		fname = tab.url.slice(tab.url.lastIndexOf('/'));
		browser.downloads.download(
			{
				url: tab.url,
				filename: `dtimgs/${fname}`
			});
		browser.tabs.remove(tab.id);
	}
}

function onErr(err) {
	console.error(err);
}

browser.browserAction.onClicked.addListener(() => {
	browser.tabs.query({currentWindow: true})
		.then(runtab, onErr);
});
