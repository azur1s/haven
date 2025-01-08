var http = (() => {
return require(`http`)
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
return `
<!DOCTYPE html>
<html lang='en'>
<head>
    <meta charset='UTF-8'>
    <meta name='viewport' content='width=device-width, initial-scale=1.0'>
    <title>Document</title>
</head>
<body>
    <h1>Hello, World!</h1>
</body>
<style>
    h1 {
        font-size: 3rem;
    }
</style>
</html>
    `
})();
var handle = ((req, res) => {
var _ = res_write(res, response_html);
return res_end(res)
});
var main = (() => {
var s = server_create(handle);
return server_start(s, 8080)
});
main()