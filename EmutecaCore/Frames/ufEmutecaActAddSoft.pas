unit ufEmutecaActAddSoft;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, strutils,Forms, Controls, StdCtrls,
  EditBtn, ActnList,
  ExtCtrls, Buttons,
  uCHXStrUtils, u7zWrapper,
  ucEmuteca, ucEmutecaSoftware, ucEmutecaSystem,
  ufEmutecaSoftEditor;

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
    eFile: TFileNameEdit;
    eVersionKey: TEdit;
    gbxFileSelection: TGroupBox;
    gbxVersionInfo: TGroupBox;
    ilActions: TImageList;
    lSystemInfo: TLabel;
    pBottom: TPanel;
    pLeft: TPanel;
    rgbVersionKey: TRadioGroup;
    procedure bAcceptClick(Sender: TObject);
    procedure cbxInnerFileChange(Sender: TObject);
    procedure cbxSystemChange(Sender: TObject);
    procedure chkOpenAsArchiveChange(Sender: TObject);
    procedure eFileAcceptFileName(Sender: TObject; var Value: string);

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

    procedure UpdateGameKey;
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

procedure TfmActAddVersion.eFileAcceptFileName(Sender: TObject;
  var Value: string);
begin
  // Updating VersionEditor
  Version.Folder := ExtractFileDir(Value);
  Version.FileName:= ExtractFileName(Value);
  Version.Parent := RemoveFromBrackets(ExtractFileNameOnly(Version.FileName));
  Version.Title := Version.Parent;
  Version.Description := CopyFromBrackets(ExtractFileNameOnly(Version.FileName));
  VersionEditor.UpdateData;

  chkOpenAsArchive.Checked:=False;
  cbxInnerFile.Clear;

  // Recognized ext of an archive (from cEmutecaConfig, not u7zWrapper)
  chkOpenAsArchive.Enabled:= SupportedExt(Value, Emuteca.Config.CompressedExtensions);
  cbxInnerFile.Enabled:= chkOpenAsArchive.Enabled;
end;

procedure TfmActAddVersion.bAcceptClick(Sender: TObject);
begin
  VersionEditor.SaveData;
  Emuteca.SoftManager.FullList.Add(Version);
  // HACK: Created a new version, so we can free it on Destroy.
  FVersion := cEmutecaVersion.Create(nil);
end;

procedure TfmActAddVersion.cbxInnerFileChange(Sender: TObject);
begin
  Version.Folder := eFile.Text;
  Version.FileName:= cbxInnerFile.Text;
  Version.Parent := RemoveFromBrackets(ExtractFileNameOnly(eFile.Text));
  Version.Title := RemoveFromBrackets(ExtractFileNameOnly(cbxInnerFile.Text));
  Version.Description := CopyFromBrackets(ExtractFileNameOnly(cbxInnerFile.Text));
  VersionEditor.UpdateData;
end;

procedure TfmActAddVersion.cbxSystemChange(Sender: TObject);
var
  TempSys: cEmutecaSystem;
  ExtFilter: string;
begin
  if cbxSystem.ItemIndex = -1 then Exit;

  TempSys := cEmutecaSystem(cbxSystem.Items.Objects[cbxSystem.ItemIndex]);

  if TempSys = nil then Exit;

  Version.System := TempSys.ID;

  // Autoselecting Key Type
  case TempSys.GameKey of
     TEFKCRC32: rgbVersionKey.ItemIndex:=1;
     TEFKCustom: rgbVersionKey.ItemIndex:=2;
     TEFKFileName: rgbVersionKey.ItemIndex:=3;
    else  // SHA1 by default
      rgbVersionKey.ItemIndex:=0;
  end;
  UpdateGameKey;

  lSystemInfo.Caption := TempSys.Extensions.CommaText;

  ExtFilter := 'All suported files|';
  ExtFilter := ExtFilter + '*.' + AnsiReplaceText(lSystemInfo.Caption,',',';*.');
  ExtFilter := ExtFilter + ';*.' + AnsiReplaceText(w7zFileExts,',',';*.');
  ExtFilter := ExtFilter + '|All files|' + AllFilesMask;
  eFile.Filter:=ExtFilter;
end;

procedure TfmActAddVersion.chkOpenAsArchiveChange(Sender: TObject);
begin
  if not chkOpenAsArchive.Enabled then Exit;

  if not SupportedExt(eFile.Text, Emuteca.Config.CompressedExtensions) then Exit;

  cbxInnerFile.Clear;

  if not FileExistsUTF8(eFile.Text) then Exit;

  w7zListFiles(eFile.Text, cbxInnerFile.Items, True)

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

  VersionEditor.Emuteca := Emuteca;
end;

procedure TfmActAddVersion.SetVersion(AValue: cEmutecaVersion);
begin
  if FVersion = AValue then
    Exit;
  FVersion := AValue;
end;

procedure TfmActAddVersion.UpdateGameKey;
begin
  // We use selected rgbVersionKey, not system default
   case rgbVersionKey.ItemIndex of
     1: {TEFKCRC32};
     2: {TEFKCustom} eVersionKey.Clear;
     3: {TEFKFileName} eVersionKey.Text := SetAsID(Version.FileName);
    else  // TEFKSHA1 by default
      ;
  end;

   Version.ID := eVersionKey.Text;
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
