function main() {
  var openpgp = window.openpgp;
  openpgp.initWorker({ path: '/static/js/openpgp.worker.min.js' });

  var refs = {
    submit: document.getElementById('submit'),
    privateKey: document.getElementById('private-key'),
    postForm: document.getElementById('post-form')
  };

  refs.submit.addEventListener('click', function (e) {
    e.preventDefault();
    privateKey = refs.privateKey.value;
  });
}

if (document.readyState === "loading") {
  document.addEventListener("DOMContentLoaded", main);
} else {
  main();
}
