// Downloads all namebase-listed domains in json
//
// Paste into browser console...
//
// Original script from https://github.com/HDardenne/nb-scripts/blob/main/list-tlds.js

(async function () {
  const timeDelay = 1250;
  let domains = [];
  let done = false;
  let index = 0;

  while (!done) {
    const resp = await fetch(
      'https://www.namebase.io/api/user/domains/listed/' + index + '?limit=100',
      {
        credentials: 'include',
        headers: {
          Accept: 'application/json',
          'Content-Type': 'application/json',
          'Cache-Control': 'max-age=0',
        },
        referrer: 'https://www.namebase.io/manage/owned?page=1',
        method: 'GET',
        mode: 'cors',
      }
    );
    const json = await resp.json();
    const newDomains = json.domains.map(a => '{"name": "' + a.name + '", "amount": "' + a.amount + '"}');
    domains = domains.concat(newDomains);
    if (newDomains.length === 0) {
      done = true;
    } else {
      index += newDomains.length;
      await new Promise(res => setTimeout(() => res(), timeDelay));
    }
  }

  const hiddenLink = document.createElement('a');
  hiddenLink.href = 'data:text/txt;charset=utf-8,' + encodeURI('{"domains":[\n' + domains.join(',\n') + ']}\n');
  hiddenLink.target = '_blank';
  hiddenLink.download = `listed.json`;
  hiddenLink.click();
  hiddenLink.remove();
})();
