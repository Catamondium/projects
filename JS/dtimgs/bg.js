var re = /.+(\.jpg|\.jpeg|\.png)$/i;
function runtab(tabs) {
	let imgtabs = tabs.filter(tab => {
		let check = new URL(tab.url);
		return re.test(check.pathname);
	});
	for(let tab of imgtabs) {
		console.log(`Found: ${tab.url}`);
		let path = new URL(tab.url);
		fname = path.pathname.slice(1);
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
