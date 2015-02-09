# den alten Webserver wegwerfen
killall lighttpd

# webserver.hs kompilieren (und webserver.cgi nennen, damit es von lighttpd gefunden wird)
ghc --make -O2 -o webserver.cgi -outputdir ghc_outputdir webserver.hs

# einen neuen Webserver hinstellen mit der richtigen config
lighttpd -f lighttpd.conf
