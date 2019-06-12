const DEBUG = false;
var re = /.+\.(jpg|jpeg|png)$/i;
function runtab(tabs) {
    let imgtabs = tabs.filter(tab => {
        let check = new URL(tab.url);
        return re.test(check.pathname);
    });
    for (let tab of imgtabs) {
        let path = new URL(tab.url);
        fname = path.pathname;
        fname = fname.slice(fname.lastIndexOf('/') + 1); // flatten name
        console.log(`Found: ${fname}`);
        if (!DEBUG) {
            chrome.downloads.download(
                {
                    url: tab.url,
                    filename: `dtimgs/${fname}`
                });
            chrome.tabs.remove(tab.id);
        }
    }
}

function onErr(err) {
    console.error(err);
}

chrome.browserAction.onClicked.addListener(() => {
    chrome.tabs.query({ currentWindow: true }, (tab) => runtab(tab))
});
