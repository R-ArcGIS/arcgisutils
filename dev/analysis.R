arc_portal_self()

urls <- arc_portal_urls()

sort(names(urls$helperServices))
sort(names(urls))

urls$helperServices$geometry

pself <- arc_portal_self(token = auth_user())

sprintf("https://%s", pself$customBaseUrl)
pself$id
