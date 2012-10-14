unit uCHXFileUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, crc, FileUtil,
  uCHXStrUtils;

type
  TItFolderObj = function(Folder: String;
    FileInfo: TSearchRec): boolean of object;
  TItFolderFun = function(Folder: String; FileInfo: TSearchRec): boolean;
  
{ RemoveDir does the job...
procedure RemoveEmptyFolders(aPath: string);
}

function CRC32File(const aFileName: String): cardinal;
{< Calculates the CRC32 checksum of a file.
}

function IterateFolderObj(Folder: String; aFunction: TItFolderObj;
  Recursive: boolean = True): boolean;
function IterateFolderFun(Folder: String; aFunction: TItFolderFun;
  Recursive: boolean = True): boolean;
{< Recorre el directorio especificado y ejecuta aFuncion(TSearchRec) con cada uno
  de los archivos encontrados

Parámetros:
  - Directorio: string -> Directorio a recorrer
  - Funcion: function(Directorio: string; Info: TSearchRec): Boolean -> Metodo
    de un objeto al que se le pasa el TSearchRec y el directorio del archivo
    actual como parámetro para operar con él. Si devuelve True continua con el
    siguiente fichero.
  - Recursivo: Boolean -> Indica si también se deben recorrer los subdirectorios
  - Result: Boolean -> Indica si se ha continuar la operación

Notas:
  - Si Funcion devuelve True continúa recorriendo el directorio, si devuelve
    False no pasa al siguiente archivo.
}

function FilesInFolder(Folder: String): integer;
//< TODO 2: Is there a better way?

implementation

function CRC32File(const aFileName: String): cardinal;
var
  aFile: TFileStream;
  BufferCRC: array[0..32767] of char;
  BufferSize: cardinal;
begin
  BufferCRC[0] := #32; // Fix inicialization warning
  BufferSize := SizeOf(BufferCRC);
  Result := crc32(0, nil, 0);

  if not FileExistsUTF8(aFileName) then
    Exit;

  aFile := TFileStream.Create(UTF8ToSys(aFileName), fmOpenRead);
  try
    aFile.Position := 0;

    while (aFile.Position < aFile.Size) do
    begin
      if (aFile.Size - aFile.Position) < BufferSize then
        BufferSize := aFile.Size - aFile.Position;
      aFile.ReadBuffer(BufferCRC, BufferSize);
      Result := crc32(Result, @BufferCRC, BufferSize);
    end;
  finally
    FreeAndNil(aFile);
  end;
end;

function IterateFolderObj(Folder: String; aFunction: TItFolderObj;
  Recursive: boolean = True): boolean;
var
  Info: TSearchRec;
begin
  Result := True;
  Folder := SetAsFolder(Folder);
  if (Folder = '') or (not DirectoryExistsUTF8(Folder)) then
    Exit;

  if FindFirstUTF8(Folder + AllFilesMask, faAnyFile, Info) = 0 then
    try
      repeat
        Result := aFunction(Folder, Info);
      until (FindNextUTF8(Info) <> 0) or not Result;
    finally
      FindCloseUTF8(Info);
    end;

  if Recursive and Result then
    if FindFirstUTF8(Folder + AllFilesMask, faDirectory, Info) = 0 then
      try
        repeat
          if (Info.Name <> '.') and (Info.Name <> '') and
            (Info.Name <> '..') and
            ((Info.Attr and faDirectory) <> 0) then
            Result := IterateFolderObj(Folder + Info.Name, aFunction, True);
        until (FindNextUTF8(Info) <> 0) or not Result;
      finally
        FindCloseUTF8(Info);
      end;
end;

function IterateFolderFun(Folder: String; aFunction: TItFolderFun;
  Recursive: boolean): boolean;
var
  Info: TSearchRec;
begin
  Result := True;
  Folder := SetAsFolder(Folder);
  if (Folder = '') or (not DirectoryExistsUTF8(Folder)) then
    Exit;

  if FindFirstUTF8(Folder + AllFilesMask, faAnyFile, Info) = 0 then
    try
      repeat
        Result := aFunction(Folder, Info);
      until (FindNextUTF8(Info) <> 0) or not Result;
    finally
      FindCloseUTF8(Info);
    end;

  if Recursive and Result then
    if FindFirstUTF8(Folder + AllFilesMask, faDirectory, Info) = 0 then
      try
        repeat
          if (Info.Name <> '.') and (Info.Name <> '') and
            (Info.Name <> '..') and
            ((Info.Attr and faDirectory) <> 0) then
            Result := IterateFolderFun(Folder + Info.Name, aFunction, True);
        until (FindNextUTF8(Info) <> 0) or not Result;
      finally
        FindCloseUTF8(Info);
      end;
end;

function FilesInFolder(Folder: String): integer;
var
  Info: TSearchRec;
begin
  // Podría usar IterateFolderObj pero no es plan de complicar la cosa
  Result := 0;
  Folder := SetAsFolder(Folder);

  if FindFirstUTF8(Folder + '*', 0, Info) = 0 then
    try
      repeat
        Inc(Result);
      until (FindNextUTF8(Info) <> 0);
    finally
      FindCloseUTF8(Info);
    end;
end;

end.

