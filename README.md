# coupon-servant
## Setup
* put the postgres connection string in `Main.hs`
* run `stack build` then
* run `stack exec coupon-servant-exe` to start the server


## Testing
* run `docker pull swaggerapi/swagger-ui` to download swagger-ui container
* run `docker run -d --name swui -e API_URL="http://localhost:3000/swagger.json" -p 5341:8080/tcp swaggerapi/swagger-ui`
* point your browser to [swagger-ui](http://localhost:5341)