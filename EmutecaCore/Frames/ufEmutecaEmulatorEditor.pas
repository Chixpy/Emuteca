unit ufEmutecaEmulatorEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, ActnList, StdCtrls, EditBtn, Spin, Menus, LazFileUtils, LCLIntf,
  uCHXStrUtils,
  ufCHXPropEditor,
  ucEmutecaEmulator;

type

  { TfmEmutecaEmulatorEditor }

  TfmEmutecaEmulatorEditor = class(TfmCHXPropEditor)
    actOpenWebPage: TAction;
    actWFEmulator: TAction;
    actWFROM: TAction;
    bGoWebPage: TSpeedButton;
    bParameters: TSpeedButton;
    eDeveloper: TEdit;
    eExePath: TFileNameEdit;
    eExitCode: TSpinEdit;
    eName: TEdit;
    eParameters: TEdit;
    eWebPage: TEdit;
    eWorkingFolder: TDirectoryEdit;
    lDeveloper: TLabel;
    lExePath: TLabel;
    lExitCode: TLabel;
    lExtensions: TLabel;
    lID: TLabel;
    lName: TLabel;
    lParameters: TLabel;
    lWebPage: TLabel;
    lWorkingFolder: TLabel;
    pmiWFEmu: TMenuItem;
    pmiWFROM: TMenuItem;
    mExtensions: TMemo;
    pmParameters: TPopupMenu;
    pmWFolder: TPopupMenu;
    pParameters: TPanel;
    pWebPage: TPanel;
    pWFolder: TPanel;
    bWorkingFolder: TSpeedButton;
    procedure actOpenWebPageExecute(Sender: TObject);
    procedure actWFEmulatorExecute(Sender: TObject);
    procedure actWFROMExecute(Sender: TObject);
    procedure bParametersClick(Sender: TObject);
    procedure bWorkingFolderClick(Sender: TObject);
    procedure eFileButtonClick(Sender: TObject);

  private
    FEmulator: cEmutecaEmulator;
    procedure SetEmulator(AValue: cEmutecaEmulator);

  protected
    procedure DoClearFrameData;
    procedure DoLoadFrameData;
    procedure DoSaveFrameData;

  public
    property Emulator: cEmutecaEmulator read FEmulator write SetEmulator;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

  end;

implementation

{$R *.lfm}

{ TfmEmutecaEmulatorEditor }

constructor TfmEmutecaEmulatorEditor.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  OnClearFrameData := @DoClearFrameData;
  OnLoadFrameData := @DoLoadFrameData;
  OnSaveFrameData := @DoSaveFrameData;
end;

destructor TfmEmutecaEmulatorEditor.Destroy;
begin
  inherited Destroy;
end;

procedure TfmEmutecaEmulatorEditor.actWFEmulatorExecute(Sender: TObject);
begin
  eWorkingFolder.Text := kEmutecaEmuDirKey;
end;

procedure TfmEmutecaEmulatorEditor.actOpenWebPageExecute(Sender: TObject);
begin
  if eWebPage.Text = '' then Exit;
  OpenURL(eWebPage.Text);
end;

procedure TfmEmutecaEmulatorEditor.actWFROMExecute(Sender: TObject);
begin
  eWorkingFolder.Text := kEmutecaRomDirKey;
end;

procedure TfmEmutecaEmulatorEditor.bParametersClick(Sender: TObject);
begin
  pmParameters.PopUp;
end;

procedure TfmEmutecaEmulatorEditor.bWorkingFolderClick(Sender: TObject);
begin
  pmWFolder.PopUp;
end;

procedure TfmEmutecaEmulatorEditor.eFileButtonClick(Sender: TObject);
var
  aEFN: TFileNameEdit;
begin
  aEFN := TFileNameEdit(Sender);
  if FilenameIsAbsolute(aEFN.FileName) then
  begin
    aEFN.InitialDir := ExtractFileDir(SysPath(aEFN.FileName));
  end
  else
  begin
    aEFN.InitialDir := ExtractFileDir(TrimFilename(ProgramDirectory +
      aEFN.FileName));
  end;
end;

procedure TfmEmutecaEmulatorEditor.SetEmulator(AValue: cEmutecaEmulator);
begin
  if FEmulator = AValue then
    Exit;
  FEmulator := AValue;
  LoadFrameData;
end;

procedure TfmEmutecaEmulatorEditor.DoClearFrameData;
begin
  lID.Caption := ' ';
  eName.Clear;
  eDeveloper.Clear;
  eWebPage.Clear;
  eExePath.Clear;
  eWorkingFolder.Clear;
  eParameters.Clear;
  mExtensions.Clear;
  eExitCode.Value := 0;
end;

procedure TfmEmutecaEmulatorEditor.DoLoadFrameData;
begin
  ClearFrameData;

  Enabled := assigned(Emulator);

  if not Enabled then
    Exit;

  lID.Caption := Emulator.ID;
  eName.Text := Emulator.EmulatorName;
  eDeveloper.Text := Emulator.Developer;
  eWebPage.Text := Emulator.WebPage;

  eExePath.Text := SysPath(Emulator.ExeFile);
  eWorkingFolder.Text := SysPath(Emulator.WorkingFolder);
  eParameters.Text := Emulator.Parameters;
  mExtensions.Lines.Assign(Emulator.FileExt);
  eExitCode.Value := Emulator.ExitCode;
end;

procedure TfmEmutecaEmulatorEditor.DoSaveFrameData;
begin
  Emulator.EmulatorName := eName.Text;
  Emulator.Developer := eDeveloper.Text;
  Emulator.WebPage := eWebPage.Text;

  Emulator.ExeFile := eExePath.Text;
  Emulator.WorkingFolder := eWorkingFolder.Text;
  Emulator.Parameters := eParameters.Text;
  Emulator.FileExt.Assign(mExtensions.Lines);
  Emulator.ExitCode := eExitCode.Value;
end;

end.
