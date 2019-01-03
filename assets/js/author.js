function main() {
  var openpgp = window.openpgp;
  openpgp.initWorker({ path: '/static/js/openpgp.worker.min.js' });

  publicKeyArmored = window.PUBLIC_KEY;

  var refs = {
    submit: document.getElementById('submit'),
    privateKey: document.getElementById('private-key'),
    passphrase: document.getElementById('passphrase'),
    postForm: document.getElementById('post-form')
  };

  refs.submit.addEventListener('click', (e) => {
    e.preventDefault();
    var privateKeyArmored = refs.privateKey.value;
    var passphrase = refs.passphrase.value;
    var title = refs.postForm.title.value;
    var body = refs.postForm.body.value;
    var clearText = openpgp.cleartext.fromText(title + body);

    var privateKey = null;
    var publicKey = null;
    var detachedSignature = null;

    openpgp.key.readArmored(privateKeyArmored).then(({ err, keys }) => {
      if (err) {
        throw 'Failed to read private key with ' + err[0];
      }

      privateKey = keys[0];
      return privateKey.decrypt(passphrase);
    }).then(success => {
      if (!success) {
        throw 'Failed to decrypt private key with provided passphrase';
      }

      options = {
        message: clearText,
        privateKeys: [privateKey],
        detached: true
      };

      return openpgp.sign(options);
    }).then(signed => {
      detachedSignature = signed.signature;

      return Promise.all([openpgp.signature.readArmored(detachedSignature), openpgp.key.readArmored(publicKeyArmored)]);
    }).then(([signature, {err, keys}]) => {
      if (err) {
        throw 'Failed to read public key with ' + err[0];
      }

      publicKey = keys[0];
      options = {
        message: clearText,
        signature: signature,
        publicKeys: [publicKey]
      };

      return openpgp.verify(options);
    }).then(verified => {
      if (!verified.signatures[0].valid) {
        throw 'Failed to verify signature. Are you using the right private key?';
      }

      return fetch('/posts', {
        method: 'POST',
        body: JSON.stringify({signature: detachedSignature, data: {title: title, body: body}}),
        headers: {
          'Content-Type': 'application/json',
        }
      });
    }).catch(failure => {
      alert(failure);
    });
  });
}

if (document.readyState === "loading") {
  document.addEventListener("DOMContentLoaded", main);
} else {
  main();
}
