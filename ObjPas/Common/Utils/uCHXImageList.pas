{ cImageList unit. }
unit uCHXImageList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, FileUtil, fgl;

type

  { cImageList }
  cImageList = class (specialize TFPGObjectList<TPicture>)
  public
    function AddImageFile(aFile: String): Integer;
    function AddEmptyImage: Integer;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ cImageList }

function cImageList.AddImageFile(aFile: String): Integer;
var
  Img: TPicture;
begin
  Result := -1;
  if not FileExistsUTF8(aFile) then Exit;
  Img := TPicture.Create;
  try
    Img.LoadFromFile(aFile);
  except
    // WOOPS, it can't be loaded.
    FreeAndNil(Img);
    Exit;
  end;
  Result := Self.Add(Img);
end;

function cImageList.AddEmptyImage: Integer;
var
  aImage: TPicture;
begin
  aImage := TPicture.Create;
  Result := Self.Add(aImage);
end;

constructor cImageList.Create;
begin
  Inherited Create;
end;

destructor cImageList.Destroy;
begin
  inherited Destroy;
end;

end.

