unit ufEmutecaActAddVersion;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, strutils,Forms, Controls, StdCtrls,
  EditBtn, ActnList,
  ExtCtrls, Buttons,
  uCHXStrUtils, u7zWrapper,
  ucEmuteca, ucEmutecaVersion, ucEmutecaSystem,
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
    procedure cbxSystemChange(Sender: TObject);
    procedure chkOpenAsArchiveChange(Sender: TObject);
    procedure eArchiveAcceptFileName(Sender: TObject; var Value: string);

  private
    FEmuteca: cEmuteca;
    FIconsIni: string;
    FVersion: cEmutecaVersion;
    FVersionEditor: TfmEmutecaVersionEditor;
    procedure SetEmuteca(AValue: cEmuteca);
    procedure SetIconsIni(AValue: string);
    procedure SetVersion(AValue: cEmutecaVersion);

  protected
    property VersionEditor: TfmEmutecaVersionEditor read FVersionEditor;
    procedure UpdateLists;


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
  var Value: string);
begin
  Version.Folder := ExtractFileDir(Value);
  Version.FileName:= ExtractFileNameOnly(Value);
  VersionEditor.cbxParent.Text :=
    RemoveFromBrackets(ExtractFileNameOnly(Version.FileName));
  VersionEditor.eTitle.Text := VersionEditor.cbxParent.Text;
  VersionEditor.eDescription.Text :=
    CopyFromBrackets(ExtractFileNameOnly(Version.FileName));

  if cbxSystem.ItemIndex = -1 then Exit;

  chkOpenAsArchive.Checked:=False;
  cbxInnerFile.Clear;

  // Recognized ext of an archive (from cEmutecaConfig, not u7zWrapper)
  chkOpenAsArchive.Enabled:= SupportedExt(Value, Emuteca.Config.CompressedExtensions);
  cbxInnerFile.Enabled:= chkOpenAsArchive.Enabled;
end;

procedure TfmActAddVersion.bAcceptClick(Sender: TObject);
begin
  Emuteca.SoftManager.FullList.Add(Version);
  FVersion := cEmutecaVersion.Create(nil);
end;

procedure TfmActAddVersion.cbxSystemChange(Sender: TObject);
var
  TempSys: cEmutecaSystem;
  ExtFilter: string;
begin
  if cbxSystem.ItemIndex = -1 then Exit;

  TempSys := cEmutecaSystem(cbxSystem.Items.Objects[cbxSystem.ItemIndex]);

  ExtFilter := 'All suported files|';
  ExtFilter := ExtFilter + '*.' + AnsiReplaceText(TempSys.Extensions.CommaText,',',';*.');
  ExtFilter := ExtFilter + ';*.' + AnsiReplaceText(w7zFileExts,',',';*.');
  ExtFilter := ExtFilter + '|All files|' + AllFilesMask;
  eArchive.Filter:=ExtFilter;

end;

procedure TfmActAddVersion.chkOpenAsArchiveChange(Sender: TObject);
begin
  if not chkOpenAsArchive.Enabled then Exit;

  if not SupportedExt(eArchive.Text, Emuteca.Config.CompressedExtensions) then Exit;

  cbxInnerFile.Clear;

  if not FileExistsUTF8(eArchive.Text) then Exit;

  w7zListFiles(eArchive.Text, cbxInnerFile.Items, True)
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
  if FEmuteca = AValue then
    Exit;
  FEmuteca := AValue;

  if not assigned(Emuteca) then exit;

  // Updating system list
  cbxSystem.Clear;
  Emuteca.SystemManager.AssingEnabledTo(cbxSystem.Items);

end;

procedure TfmActAddVersion.SetVersion(AValue: cEmutecaVersion);
begin
  if FVersion = AValue then
    Exit;
  FVersion := AValue;
end;

procedure TfmActAddVersion.UpdateLists;
begin
  cbxSystem.Clear;
  VersionEditor.Emuteca := Emuteca;

  if not assigned(Emuteca) then
    Exit;

  Emuteca.SystemManager.AssingEnabledTo(cbxSystem.Items);
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
  FreeAndNil(FVersion);
  inherited Destroy;
end;

end.
