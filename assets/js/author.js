function main() {
  var openpgp = window.openpgp;
  openpgp.initWorker({ path: '/static/js/openpgp.worker.min.js' });

  publicKeyArmored = window.PUBLIC_KEY;
  openpgp.key.readArmored(publicKeyArmored).then(({err, keys}) => {
    if (err) {
      throw 'Failed to read public key with ' + err[0];
    }

    run(openpgp, keys[0]);
  }).catch((failure) => {
    alert(failure);
  });
}

function run(openpgp, publicKey) {
  var refs = {
    submits: document.querySelectorAll('.button'),
    privateKey: document.getElementById('private-key'),
    passphrase: document.getElementById('passphrase'),
    toggles : document.querySelectorAll('.toggle')
  };

  function sign(clearText) {
    return new Promise((resolve, reject) => {
      var privateKeyArmored = refs.privateKey.value;
      var passphrase = refs.passphrase.value;
      var message = openpgp.cleartext.fromText(clearText);
      var privateKey = null;
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
          message: message,
          privateKeys: [privateKey],
          detached: true
        };

        return openpgp.sign(options);
      }).then(signed => {
        detachedSignature = signed.signature;

        return openpgp.signature.readArmored(detachedSignature);
      }).then((signature) => {
        options = {
          message: message,
          signature: signature,
          publicKeys: [publicKey]
        };

        return openpgp.verify(options);
      }).then(verified => {
        if (!verified.signatures[0].valid) {
          throw 'Failed to verify signature. Are you using the right private key?';
        }

        resolve(detachedSignature);
      }).catch((failure) => {
        reject(failure);
      });
    });
  }

  function send(method, endpoint, detachedSignature, data) {
    return new Promise((resolve, reject) => {
      var status = null;
      var ok = null;
      fetch(endpoint, {
        method: method,
        body: JSON.stringify({signature: detachedSignature, data: data}),
        headers: {
          'Content-Type': 'application/json',
        }
      }).then((response) => {
        ok = response.ok;
        status = response.status;
        return response.json();
      }).then((body) => {
        if (!ok) {
          throw `request to ${endpoint} failed: ${status} - ${body['error']['message']}`;
        }

        resolve(body);
      }).catch((failure) => {
        reject(failure);
      });
    });
  }

  function signAndSend(endpoint, clearText, data, method) {
    return new Promise((resolve, reject) => {
      sign(clearText).then((detachedSignature) => {
        return send((typeof method === 'undefined') ? 'POST' : method, endpoint, detachedSignature, data);
      }).then((_) => {
        alert('The request was signed and sent successfully!');
      }).catch((failure) => {
        alert(failure);
      });
    });
  }

  var formHandlers = {
    'post': (form) => {
      var title = form.title.value.trim();
      var body = form.body.value.trim();
      signAndSend('/posts', title + body, {'title': title, 'body': body});
    },
    'editpost': (form) => {
      var id = form.postId.value.trim();
      var title = form.title.value.trim();
      var body = form.body.value.trim();
      signAndSend(`/posts/${id}`, title + body, {'title': title, 'body': body}, 'PUT');
    },
    'deletepost': (form) => {
      var id = form.postId.value.trim();
      var b = confirm('Confirm post deletion.');
      if (b) {
        signAndSend(`/posts/${id}`, id, null, 'DELETE');
      }
    },
    'about': (form) => {
      var title = form.title.value.trim();
      var body = form.body.value.trim();
      signAndSend('/about', title + body, {'title': title, 'body': body});
    },
    'contact': (form) => {
      var location = form.location.value.trim();
      var email = form.email.value.trim();
      var linkedIn = form.linkedIn.value.trim();
      var facebookMessenger = form.facebookMessenger.value.trim();
      var instagram = form.instagram.value.trim();
      signAndSend('/contact', location + email + linkedIn + facebookMessenger + instagram, {
        'location': location,
        'email': email,
        'linked_in': linkedIn,
        'facebook_messenger': facebookMessenger,
        'instagram': instagram
      });
    }
  };

  refs.toggles.forEach((toggle) => {
    toggle.addEventListener('click', (e) => {
      e.target.nextSibling.classList.toggle('hidden');
    });
  });

  refs.submits.forEach((submit) => {
    submit.addEventListener('click', (e) => {
      e.preventDefault();
      var form = e.target.parentElement;
      var key = form.id.split('-')[0];
      if (e.target.innerHTML === 'delete') {
        key = 'deletepost';
      }
      formHandlers[key](form);
    });
  });
}

if (document.readyState === "loading") {
  document.addEventListener("DOMContentLoaded", main);
} else {
  main();
}
