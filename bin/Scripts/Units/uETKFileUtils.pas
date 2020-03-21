{
[Info]
Some common functions for file handling.
[Data]
Name=Chixpy
Version=0.01
Date=20171205
[Changes]
0.01
  + ETKCheckRenameFile
[EndInfo]
}

function ETKCheckRenameFile(const aFile: string): string;
// Checks if a file already exists, and change its name adding '(x)'
var
  j: integer;
  aFilePath, aFilename, aFileExt: string;
begin
   Result := aFile;
   aFilePath := ExtractFilePath(aFile);
   aFilename := ExtractFilenameOnly(aFile);
   aFileExt := ExtractFileExt(aFile);
   
   j := 1;
   while FileExists(Result) do
   begin
     Result := aFilePath + aFilename + ' (' +  IntToStr(j) + ')' + aFileExt;
     Inc(j);
   end;
end;