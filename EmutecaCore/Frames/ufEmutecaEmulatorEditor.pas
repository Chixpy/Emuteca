unit ufEmutecaEmulatorEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, ActnList, StdCtrls, EditBtn, Spin, Menus, LazFileUtils,
  uCHXStrUtils,
  ufCHXPropEditor,
  ucEmutecaEmulator;

type

  { TfmEmutecaEmulatorEditor }

  TfmEmutecaEmulatorEditor = class(TfmCHXPropEditor)
    actWFEmulator: TAction;
    actWFROM: TAction;
    bParameters: TSpeedButton;
    eExePath: TFileNameEdit;
    eExitCode: TSpinEdit;
    eName: TEdit;
    eParameters: TEdit;
    eWorkingFolder: TDirectoryEdit;
    lExePath: TLabel;
    lExitCode: TLabel;
    lExtensions: TLabel;
    lID: TLabel;
    lName: TLabel;
    lParameters: TLabel;
    lWorkingFolder: TLabel;
    pmiWFEmu: TMenuItem;
    pmiWFROM: TMenuItem;
    mExtensions: TMemo;
    pmParameters: TPopupMenu;
    pmWFolder: TPopupMenu;
    pParameters: TPanel;
    pWFolder: TPanel;
    bWorkingFolder: TSpeedButton;
    procedure actWFEmulatorExecute(Sender: TObject);
    procedure actWFROMExecute(Sender: TObject);
    procedure bParametersClick(Sender: TObject);
    procedure bWorkingFolderClick(Sender: TObject);
    procedure eFileButtonClick(Sender: TObject);
  private
    FEmulator: cEmutecaEmulator;
    procedure SetEmulator(AValue: cEmutecaEmulator);
    { private declarations }

  protected
    procedure ClearData; override;

  public
    { public declarations }
    procedure LoadData; override;
    procedure SaveData; override;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

  published
    property Emulator: cEmutecaEmulator read FEmulator write SetEmulator;
  end;

implementation

{$R *.lfm}

{ TfmEmutecaEmulatorEditor }

constructor TfmEmutecaEmulatorEditor.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

destructor TfmEmutecaEmulatorEditor.Destroy;
begin
  inherited Destroy;
end;

procedure TfmEmutecaEmulatorEditor.actWFEmulatorExecute(Sender: TObject);
begin
  eWorkingFolder.Text := kEmutecaEmuDirKey;
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
  LoadData;
end;

procedure TfmEmutecaEmulatorEditor.ClearData;
begin
  lID.Caption := ' ';
  eName.Clear;
  eExePath.Clear;
  eWorkingFolder.Clear;
  eParameters.Clear;
  mExtensions.Clear;
  eExitCode.Value := 0;
end;

procedure TfmEmutecaEmulatorEditor.LoadData;
begin
  ClearData;

  self.Enabled := assigned(Emulator);

  if not self.Enabled then
    Exit;

  lID.Caption := Emulator.ID;
  eName.Text := Emulator.EmulatorName;
  eExePath.Text := SysPath(Emulator.ExeFile);
  eWorkingFolder.Text := SysPath(Emulator.WorkingFolder);
  eParameters.Text := Emulator.Parameters;
  mExtensions.Lines.Assign(Emulator.FileExt);
  eExitCode.Value := Emulator.ExitCode;
end;

procedure TfmEmutecaEmulatorEditor.SaveData;
begin
  Emulator.EmulatorName := eName.Text;
  Emulator.ExeFile := eExePath.Text;
  Emulator.WorkingFolder := eWorkingFolder.Text;
  Emulator.Parameters := eParameters.Text;
  Emulator.FileExt.Assign(mExtensions.Lines);
  Emulator.ExitCode := eExitCode.Value;
end;

end.
