## mirage-block-ccm
[![Build Status](https://travis-ci.org/sg2342/mirage-block-ccm.svg?branch=master)](https://travis-ci.org/sg2342/mirage-block-ccm)

AES-CCM encrypted Mirage V1.BLOCK storage

uses two sectors of the underlying V1.BLOCK per provided sector:

```
+-----------------------------------+
| CT                | nonce | adata |
+-----------------+-----------------+
| sector n        | sector n+1      |
+-----------------+-----------------+
```

- `CT` is `sector_size + maclen` bytes AES-CCM ciphertext
- `nonce` is `nonce_len` bytes random nonce
- `adata` is `sector_size - nonce_len - maclen` random additional authenticated data

### Notes on bisect and ounit tests

```
$ CONFIGUREFLAGS="--enable-tests --enable-coverage" make distclean cover_test
```

### convert AES-CCM encrypted disk images from/to unencrypted images

```
$ CONFIGUREFLAGS="--enable-examples" make distclean build && ./ccmblock.native --help
```

