import requests

url = 'https://perso.esiee.fr/~vanrooit/R/'
print("Début téléchargement fichier 1/2")
r = requests.get(url+"players.csv", allow_redirects=True)

open('data/players.csv', 'wb').write(r.content)
print("Fin téléchargement fichier 1/2")

print("Début téléchargement fichier 2/2")
r = requests.get(url+"results.csv", allow_redirects=True)

open('data/results.csv', 'wb').write(r.content)
print("Fin téléchargement fichier 2/2")