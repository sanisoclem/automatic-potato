# TBD

Example fullstack purescript app using: purescript + halogen + cloudflare + tailwind + vite


```bash
# compile purescript
$ spago build --watch

# run worker locally - KV namespace must exist, will start vite and proxy it
$  wrangler pages dev --kv EB --port 7448 -- pnpm dev

# run dev server - runs UI only, proxying requests will not work due to the redirects/callback
$ pnpm dev
```


Example `.dev.vars`:
```
AUTH_GITHUB_CLIENT_ID = "8e5441220c7ead584d98"
AUTH_GITHUB_CLIENT_SECRET = ""
AUTH_GITHUB_CALLBACK_URI = "http://localhost:7448/api/auth/github/callback"
AUTH_JWT_PRIVATE_KEY = "PKCS8"
AUTH_JWT_PUBLIC_KEY = "SPKI"
AUTH_JWT_ISSUER = "whatever"
AUTH_JWT_AUDIENCE = "whatever"
```

To generate the keys:
```bash
# TODO: link stackoverflow answer
# generate private key
$ openssl ecparam -name secp384r1 -genkey -noout -out sec1_ec_p384_private.pem
# store in pkcs8
$ openssl pkcs8 -topk8 -nocrypt -in sec1_ec_p384_private.pem -out ec_p384_private.pem
# calc public key
$ openssl ec -in ec_p384_private.pem -pubout -out ec_p384_public.pem
$ rm sec1_ec_p384_private.pem
```