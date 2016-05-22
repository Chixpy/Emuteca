unit ucEmutecaFile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl;

type

  { cEmutecaFile }

  cEmutecaFile = class(TComponent)
  private
    FFileName: string;
    FFolder: string;
    FTitle: string;
    procedure SetFileName(AValue: string);
    procedure SetFolder(AValue: string);
    procedure SetTitle(AValue: string);

  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

  published
    property Title: string read FTitle write SetTitle;
    {< Original title of the software.

    Ideally it will contain cyrilic, korean and other fancy characters...
    }
    property Folder: string read FFolder write SetFolder;
    {< Folder (or compressed) file where the file is in. }

    property FileName: string read FFileName write SetFileName;
    {< Actual file of software.

    If the software has many files, it's the file used as reference.
    }
  end;

  { cEmutecaFileList }

  cEmutecaFileList = class (specialize TFPGObjectList<cEmutecaFile>)
    public
      procedure LoadFromFile(aFile: string);
  end;

implementation

{ cEmutecaFileList }

procedure cEmutecaFileList.LoadFromFile(aFile: string);
begin

end;

{ cEmutecaFile }

procedure cEmutecaFile.SetFileName(AValue: string);
begin
  if FFileName = AValue then Exit;
  FFileName := AValue;
end;

procedure cEmutecaFile.SetFolder(AValue: string);
begin
  if FFolder = AValue then Exit;
  FFolder := AValue;
end;

procedure cEmutecaFile.SetTitle(AValue: string);
begin
  if FTitle = AValue then Exit;
  FTitle := AValue;
end;

constructor cEmutecaFile.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
end;

destructor cEmutecaFile.Destroy;
begin
  inherited Destroy;
end;

end.

