import requests

url = 'https://perso.esiee.fr/~vanrooit/R/'
r = requests.get(url+"players.csv", allow_redirects=True)

open('data/players.csv', 'wb').write(r.content)

r = requests.get(url+"results.csv", allow_redirects=True)

open('data/results.csv', 'wb').write(r.content)