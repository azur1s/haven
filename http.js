var to_string = ((any) => {
return String(any)
});
var write = ((s) => {
return process.stdout.write(s)
});
var print = ((any) => {
return write(to_string(any))
});
var println = ((any) => {
var _then_ = print(any);
return print("\\n")
});
var log = ((s) => {
return console.log(s)
});
var http = (() => {
return require("http")
})();
var server_create = ((f) => {
return http.createServer(((req, res) => f(req, res)))
});
var server_start = ((s, port) => {
return s.listen(port)
});
var res_write = ((res, m) => {
return res.write(m)
});
var res_end = ((res) => {
return res.end()
});
var response_html = (() => {
return "\r\n<!DOCTYPE html>\r\n<html lang='en'>\r\n<head>\r\n    <meta charset='UTF-8'>\r\n    <meta name='viewport' content='width=device-width, initial-scale=1.0'>\r\n    <title>Document</title>\r\n</head>\r\n<body>\r\n    <h1>Hello, World!</h1>\r\n</body>\r\n<style>\r\n    h1 {\r\n        font-size: 3rem;\r\n    }\r\n</style>\r\n</html>\r\n    "
})();
var handle = ((req, res) => {
var _ = res_write(res, response_html);
return res_end(res)
});
var main = (() => {
var s = server_create(handle);
var _then_ = server_start(s, 8080);
return log("Listening to localhost:8080")
});
main()