# Automatically install Windows applications with CLI
# Resources: https://winget.ragerworks.com/
# Must be run in PowerShell, not Command Prompt
# Usage: .\collection.ps1


# Power tools
winget install --id=Microsoft.PowerToys -e ;
winget install --id=Neovim.Neovim -e ;
winget install --id=Obsidian.Obsidian -e ;
winget install --id=Microsoft.WindowsTerminal -e ;
winget install --id=Anki.Anki -e ;


# Developer tools
winget install --id=DBBrowserForSQLite.DBBrowserForSQLite -e ;
winget install --id=Microsoft.VisualStudioCode -e ;
winget install --id=Microsoft.WindowsTerminal -e ;
winget install --id=OpenJS.NodeJS.LTS -e ;
winget install --id=OSGeo.QGIS -e ;
winget install --id=Posit.RStudio -e ;
winget install --id=Python.Python.3.13 -e ;
winget install --id=RProject.R -e ;
winget install --id Git.Git -e --source winget ;
winget install --id GitHub.cli ;


# Utility tools
winget install --id=Adobe.Acrobat.Reader.64-bit -e ;
winget install --id=calibre.calibre -e ;
winget install --id=DigitalScholar.Zotero -e ;
# winget install --id=HandBrake.HandBrake -e ;
winget install --id=KeePassXCTeam.KeePassXC -e ;
winget install --id=Martchus.syncthingtray -e ;
winget install --id=yt-dlp.yt-dlp -e ;

# Basic tools
winget install --id=Mozilla.Firefox -e ;
winget install --id=TheDocumentFoundation.LibreOffice -e ;


# Dependencies
winget install --id=Gyan.FFmpeg -e ;
winget install --id=ImageMagick.ImageMagick -e ;


# Visual arts
# winget install --id=BlenderFoundation.Blender -e ;
# winget install --id=GIMP.GIMP.3 -e ;
# winget install --id=Inkscape.Inkscape -e ;
# winget install --id=KDE.Krita -e ;
# winget install --id=OpenShot.OpenShot -e


# Entertainment
winget install --id=AIMP.AIMP -e ;
# winget install --id=Apple.iTunes -e ;
winget install --id=Audacity.Audacity -e ;
# winget install --id=Spotify.Spotify -e ;
winget install --id=VideoLAN.VLC -e ;
