unit ufETKGUIScriptManager;
{< TfmETKGUIScriptManager frame unit.

  ----

  This file is part of Emuteca GUI.

  Copyright (C) 2011-2018 Chixpy

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 3 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  // CHX units
  uCHXStrUtils,
  // CHX frames
  ufCHXScriptManager,
  // CHX forms
  ufrCHXForm,
  // Emuteca Core units
  uEmutecaConst, uEmutecaRscStr,
  // Emuteca Core classes
  ucEmuteca, ucEmutecaScriptEngine;

type

  { TfmETKGUIScriptManager }

  TfmETKGUIScriptManager = class(TfmCHXScriptManager)
  private
    FEmuteca: cEmuteca;
    procedure SetEmuteca(AValue: cEmuteca);

  protected
    procedure DoClearFrameData;
    procedure DoLoadFrameData;

    procedure CreateCustomEngine; override;

  public
    property Emuteca: cEmuteca read FEmuteca write SetEmuteca;

    procedure SetBaseFolder(const aFolder: string); override;

    // Creates a form with Script Manager.
    class function SimpleForm(aEmuteca: cEmuteca; aBaseFolder: string;
      aGUIIconsIni: string; aGUIConfigIni: string): integer;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmETKGUIScriptManager }

procedure TfmETKGUIScriptManager.SetEmuteca(AValue: cEmuteca);
begin
  if FEmuteca = AValue then
    Exit;
  FEmuteca := AValue;

  LoadFrameData;
end;

procedure TfmETKGUIScriptManager.DoClearFrameData;
begin

end;

procedure TfmETKGUIScriptManager.DoLoadFrameData;
begin
  Enabled := Assigned(Emuteca);

  if not Enabled then
  begin
    ClearFrameData;
    Exit;
  end;

  if assigned(ScriptEngine) then
    cEmutecaScriptEngine(ScriptEngine).Emuteca := Emuteca;
end;

procedure TfmETKGUIScriptManager.CreateCustomEngine;
var
  aScriptEngine: cEmutecaScriptEngine;
begin
  // Setting before inherited call
  aScriptEngine := cEmutecaScriptEngine.Create;
  aScriptEngine.Emuteca := Emuteca;
  ScriptEngine := aScriptEngine;

  inherited CreateCustomEngine;
end;

procedure TfmETKGUIScriptManager.SetBaseFolder(const aFolder: string);
begin
  inherited SetBaseFolder(aFolder);
  ScriptEngine.CommonUnitFolder := SetAsFolder(aFolder) + 'Units';
end;

class function TfmETKGUIScriptManager.SimpleForm(aEmuteca: cEmuteca;
  aBaseFolder: string; aGUIIconsIni: string; aGUIConfigIni: string): integer;
var
  aForm: TfrmCHXForm;
  aFrame: TfmETKGUIScriptManager;
begin
  Result := mrNone;

  Application.CreateForm(TfrmCHXForm, aForm);
  try
    aForm.Name := 'frmETKGUIScriptManager';
    aForm.Caption := Format(krsFmtWindowCaption,
      [Application.Title, 'Script Manager']);

    aFrame := TfmETKGUIScriptManager.Create(aForm);
    aFrame.Align := alClient;

    aFrame.SetBaseFolder(aBaseFolder);
    aFrame.Emuteca := aEmuteca;

    aForm.LoadGUIConfig(aGUIConfigIni);
    aForm.LoadGUIIcons(aGUIIconsIni);
    aFrame.Parent := aForm;

    Result := aForm.ShowModal;
  finally
    aForm.Free;
  end;
end;

constructor TfmETKGUIScriptManager.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  slvFiles.Mask := krsFileMaskScript;

  OnClearFrameData := @DoClearFrameData;
  OnLoadFrameData := @DoLoadFrameData;
end;

destructor TfmETKGUIScriptManager.Destroy;
begin
  inherited Destroy;
end;

end.
