function main() {
  pres = document.querySelectorAll('pre');
  test = document.querySelector('#asdf');
  pres.forEach((pre) => {
    var code = pre.querySelector('code');
    if (code) {
      var frag = document.createDocumentFragment();
      lines = code.innerHTML.trim().split('\n');
      lines.forEach((line) => {
        el = document.createElement('code');
        el.innerHTML = line;
        frag.append(el);
      });
      pre.removeChild(code);
      pre.appendChild(frag);
    }
  });
}

if (document.readyState === "loading") {
  document.addEventListener("DOMContentLoaded", main);
} else {
  main();
}
