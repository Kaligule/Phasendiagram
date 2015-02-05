# den alten Webserver wegwerfen
killall lighttpd

# einen neuen Webserver hinstellen mit der richtigen config
lighttpd -f lighttpd.conf
