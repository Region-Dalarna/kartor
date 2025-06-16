@echo off
cd /d %~dp0

echo 🗑️ Tar bort gammal .git-mapp...
rmdir /s /q .git

echo 🚀 Initierar nytt git-repo...
git init
git remote add origin https://github.com/Region-Dalarna/kartor.git
git checkout -b main

echo ➕ Lägger till filer...
git add .

echo ✅ Gör ny commit...
git commit -m "🔁 Ny start: bara aktuella filer, utan gammal LFS-historik"

echo ☁️ Pushar till GitHub (force)...
git push origin main --force

echo 🟢 Klart!
pause
