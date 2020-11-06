{ Emuteca Script Unit
[Info]
Some common functions for file handling.
[Data]
Name=Chixpy
Version=0.02
Date=20201105
[Changes]
0.02
  + ETKCheckRenameFile(const aFile: string): string;
      Checks if a file already exists, and change its name adding '(x)'
  + TestFileName(aFilename: string): boolean;
      FileExistsUTF8 + Log to console
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

function TestFilename(aFilename: string): boolean;

begin
  Result := FileExists(aFilename);
  if not Result then
    WriteLn('The file "' + aFilename + '" not found.');
end;
