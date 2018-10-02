SHA1 of compressed files will be stored here if the experimental option in *GUI.ini* is set:

[Experimental]
GlobalCache=SHA1Cache

"SHA1Cache" is a folder name, actually. It can be any desired _existing_ folder, relative or absolute.

This option stores already scanned compressed file contents and its properties, for speed up when re-scaning them.