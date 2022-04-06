## mirage-block-ccm

AES-CCM encrypted Mirage Mirage_block.S storage

uses two sectors of the underlying Mirage_bloc.S per provided sector:

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
$ dune runtest --instrument-with bisect_ppx --force
$ bisect-ppx-report html
$ open _coverage/index.html
```

### convert AES-CCM encrypted disk images from/to unencrypted images

```
$ dune exec -- ccmblock --help
```


