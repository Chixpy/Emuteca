unit ufLEmuTKFullSystemEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, ComCtrls,
  ufCHXPropEditor,
  ucEmuteca, ucEmutecaSystem,
  ufEmutecaSystemEditor, ufEmutecaSystemImgEditor;

type

  { TfmLEmuTKFullSystemEditor }

  TfmLEmuTKFullSystemEditor = class(TfmCHXPropEditor)
    pcProperties: TPageControl;
  private
    FEmuteca: cEmuteca;
    FSysEditor: TfmEmutecaSystemEditor;
    FSysImgEditor: TfmSystemImgEditor;
    FSystem: cEmutecaSystem;
    procedure SetEmuteca(AValue: cEmuteca);
    procedure SetSystem(AValue: cEmutecaSystem);
    { private declarations }

  protected
    property SysEditor: TfmEmutecaSystemEditor read FSysEditor;
    property SysImgEditor: TfmSystemImgEditor read FSysImgEditor;

    procedure ClearData; override;
    procedure SetGUIIconsIni(AValue: string); override;

  public
    { public declarations }
    property Emuteca: cEmuteca read FEmuteca write SetEmuteca;
    property System: cEmutecaSystem read FSystem write SetSystem;

    procedure SaveData; override;
    procedure LoadData; override;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmLEmuTKFullSystemEditor }

procedure TfmLEmuTKFullSystemEditor.SetEmuteca(AValue: cEmuteca);
begin
  if FEmuteca = AValue then
    Exit;
  FEmuteca := AValue;


  if not Assigned(Emuteca) then
  begin
    SysEditor.EmuManager := nil;
    SysEditor.Config := nil;
  end
  else
  begin
    SysEditor.EmuManager := Emuteca.EmulatorManager;
    SysEditor.Config := Emuteca.Config;
  end;

  Self.Enabled := Assigned(Emuteca) and Assigned(System);
end;

procedure TfmLEmuTKFullSystemEditor.SetSystem(AValue: cEmutecaSystem);
begin
  if FSystem = AValue then
    Exit;
  FSystem := AValue;
  SysEditor.System := Self.System;
  SysImgEditor.System := Self.System;

  Self.Enabled := Assigned(Emuteca) and Assigned(System);
end;

procedure TfmLEmuTKFullSystemEditor.ClearData;
begin
  // Do nothing, frames do this.
end;

procedure TfmLEmuTKFullSystemEditor.SetGUIIconsIni(AValue: string);
begin
  inherited SetGUIIconsIni(AValue);

  SysEditor.GUIIconsIni := self.GUIIconsIni;
  SysImgEditor.GUIIconsIni := self.GUIIconsIni;
end;

procedure TfmLEmuTKFullSystemEditor.SaveData;
begin
  SysEditor.SaveData;
  SysImgEditor.SaveData;
end;

procedure TfmLEmuTKFullSystemEditor.LoadData;
begin
  ClearData;

  SysEditor.LoadData;
  SysImgEditor.LoadData;
end;

constructor TfmLEmuTKFullSystemEditor.Create(TheOwner: TComponent);

  procedure CreatePages;
  var
    aTabSheet: TTabSheet;
  begin
    aTabSheet := pcProperties.AddTabSheet;
    FSysEditor := TfmEmutecaSystemEditor.Create(aTabSheet);
    SysEditor.SaveButtons := False;
    SysEditor.ButtonClose := False;
    SysEditor.Align := alClient;
    SysEditor.Parent := aTabSheet;

    aTabSheet := pcProperties.AddTabSheet;
    FSysImgEditor := TfmSystemImgEditor.Create(aTabSheet);
    SysImgEditor.SaveButtons := False;
    SysImgEditor.ButtonClose := False;
    SysImgEditor.Align := alClient;
    SysImgEditor.Parent := aTabSheet;
  end;

begin
  inherited Create(TheOwner);

  CreatePages;
end;

destructor TfmLEmuTKFullSystemEditor.Destroy;
begin
  inherited Destroy;
end;

end.
