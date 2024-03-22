# Refresh / Create / Deploy gh-pages

# 1. checkout orphaned branch
git checkout --orphan gh-pages

# 2. generate slides
quarto render danton-csulb-data-day-2024.qmd --to revealjs

# 3. remove README and .gitignore; add slides.html then commit
git rm --cached .gitignore README.md
git add slides.html danton-csulb-data-day-2024_files
git commit -m "slides render $(date '+%FT%T')"

# 4. delete upstream `gh-pages`
git push origin --delete gh-pages

# 5. push changes and come back to `main`
git push origin gh-pages
git checkout main

# 6. remove gh-pages local branch
git branch -D gh-pages
