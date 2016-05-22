unit ufEmutecaActAddVersion;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, Forms, Controls, StdCtrls, EditBtn, ActnList,
  ExtCtrls, Buttons,
  uCHXStrUtils,
  ucEmuteca, ucEmutecaVersion,
  ufEmutecaVersionEditor;

type

  { TfmActAddVersion }

  TfmActAddVersion = class(TFrame)
    ActionList1: TActionList;
    bAccept: TBitBtn;
    bCancel: TBitBtn;
    bgxSystem: TGroupBox;
    cbxInnerFile: TComboBox;
    cbxSystem: TComboBox;
    chkOpenAsArchive: TCheckBox;
    eArchive: TFileNameEdit;
    eDatabaseFile: TFileNameEdit;
    gbxFileSelection: TGroupBox;
    gbxVersionInfo: TGroupBox;
    ilActions: TImageList;
    lSelectDatabase: TLabel;
    lSystemInfo: TLabel;
    pBottom: TPanel;
    procedure bAcceptClick(Sender: TObject);
    procedure bCancelClick(Sender: TObject);
    procedure eArchiveAcceptFileName(Sender: TObject; var Value: String);

  private
    FEmuteca: cEmuteca;
    FIconsIni: string;
    FVersion: cEmutecaVersion;
    FVersionEditor: TfmEmutecaVersionEditor;
    procedure SetEmuteca(AValue: cEmuteca);
    procedure SetIconsIni(AValue: string);
    procedure SetVersion(AValue: cEmutecaVersion);
    { private declarations }

  protected
    property VersionEditor: TfmEmutecaVersionEditor read FVersionEditor;
    procedure UpdateLists;

    procedure UpdateVersionName(Folder, Filename: string);

  public
    { public declarations }
    property IconsIni: string read FIconsIni write SetIconsIni;
    property Version: cEmutecaVersion read FVersion write SetVersion;
    property Emuteca: cEmuteca read FEmuteca write SetEmuteca;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmActAddVersion }

procedure TfmActAddVersion.eArchiveAcceptFileName(Sender: TObject;
  var Value: String);
begin
  UpdateVersionName(ExtractFileDir(Value),ExtractFileNameOnly(Value));
end;

procedure TfmActAddVersion.bAcceptClick(Sender: TObject);
begin
  ;
end;

procedure TfmActAddVersion.bCancelClick(Sender: TObject);
begin
  FreeAndNil(FVersion);
end;

procedure TfmActAddVersion.SetIconsIni(AValue: string);
begin
  if FIconsIni = AValue then
    Exit;
  FIconsIni := AValue;
  //ReadActionsIcons(IconsIni, Self.Name, '', ilActions, ActionList1);
end;

procedure TfmActAddVersion.SetEmuteca(AValue: cEmuteca);
begin
  if FEmuteca=AValue then Exit;
  FEmuteca:=AValue;
end;

procedure TfmActAddVersion.SetVersion(AValue: cEmutecaVersion);
begin
  if FVersion=AValue then Exit;
  FVersion:=AValue;
end;

procedure TfmActAddVersion.UpdateLists;
begin
  cbxSystem.Clear;
  VersionEditor.Emuteca := Emuteca;

  if not assigned(Emuteca) then
    Exit;

  Emuteca.SystemManager.AssingEnabledTo(cbxSystem.Items);
end;

procedure TfmActAddVersion.UpdateVersionName(Folder, Filename: string);
begin
  if chkOpenAsArchive.Enabled and FileExistsUTF8(Folder) then
  begin
    // Folder = 7z, File = inner filename
    Version.Folder := Folder;
    VersionEditor.cbxParent.Text := ChangeFileExt(ExtractFileNameOnly(Folder),'');
    VersionEditor.eTitle.Text:=RemoveFromBrackets(ExtractFileNameOnly(Filename));
    VersionEditor.eDescription.Text:=(ExtractFileNameOnly(Filename))
  end
  else
  begin
    Version.Folder := Folder;
    VersionEditor.cbxParent.Text := RemoveFromBrackets(ExtractFileNameOnly(Filename));
    VersionEditor.eTitle.Text:=RemoveFromBrackets(ExtractFileNameOnly(Filename));
  end;
end;

constructor TfmActAddVersion.Create(TheOwner: TComponent);

  procedure CreateFrames;
  begin
    FVersionEditor := TfmEmutecaVersionEditor.Create(gbxVersionInfo);
    VersionEditor.Parent := gbxVersionInfo;
    VersionEditor.Align := alClient;
  end;

begin
  inherited Create(TheOwner);

  CreateFrames;

  FVersion := cEmutecaVersion.Create(nil);
  VersionEditor.Version := self.Version;

end;

destructor TfmActAddVersion.Destroy;
begin
  inherited Destroy;
end;

end.
