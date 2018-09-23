unit ufETKGUIactMergeGroup;
{< TfmEEGUIactMergeGroup frame unit of Emuteca GUI.

  ----

  Copyright (C) 2011-2018 Chixpy

  This source is free software; you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 3 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
  more details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by
  writing to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
  Boston, MA 02111-1307, USA.
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  // CHX forms
  ufrCHXForm,
  // CHX frames
  ufCHXPropEditor,
  // Emuteca Core units
  uEmutecaConst, uEmutecaRscStr, uEmutecaCommon,
  // Emuteca Core classes
  ucEmutecaGroup;

type

  { TfmETKGUIactMergeGroup }

  TfmETKGUIactMergeGroup = class(TfmCHXPropEditor)
  private
    FGroup: cEmutecaGroup;
    procedure SetGroup(AValue: cEmutecaGroup);

  protected
        procedure DoClearFrameData;
    procedure DoLoadFrameData;
    procedure DoSaveFrameData;

  public
    property Group: cEmutecaGroup read FGroup write SetGroup;

    // Creates a form with AddSoft frame.
    class function SimpleForm(aGroup: cEmutecaGroup; aGUIIconsIni: string;
      aGUIConfigIni: string): integer;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmETKGUIactMergeGroup }

procedure TfmETKGUIactMergeGroup.SetGroup(AValue: cEmutecaGroup);
begin
  if FGroup=AValue then Exit;
  FGroup:=AValue;
end;

procedure TfmETKGUIactMergeGroup.DoClearFrameData;
begin

end;

procedure TfmETKGUIactMergeGroup.DoLoadFrameData;
begin

end;

procedure TfmETKGUIactMergeGroup.DoSaveFrameData;
begin

end;

class function TfmETKGUIactMergeGroup.SimpleForm(aGroup: cEmutecaGroup;
  aGUIIconsIni: string; aGUIConfigIni: string): integer;
var
  aForm: TfrmCHXForm;
  aFrame: TfmETKGUIactMergeGroup;
begin
  Result := mrNone;

  Application.CreateForm(TfrmCHXForm, aForm);
  try
    aForm.Name := 'frmETKGUIactMergeGroup';
    aForm.Caption := Format(krsFmtWindowCaption,
      [Application.Title, 'Merge group files...']);
    aForm.AutoSize := True;

    aFrame := TfmETKGUIactMergeGroup.Create(aForm);
    aFrame.SaveButtons := True;
    aFrame.ButtonClose := True;
    aFrame.Align := alClient;

    aFrame.Group := aGroup;

    aForm.LoadGUIConfig(aGUIConfigIni);
    aForm.LoadGUIIcons(aGUIIconsIni);
    aFrame.Parent := aForm;

    Result := aForm.ShowModal;
  finally
    aForm.Free;
  end;
end;

constructor TfmETKGUIactMergeGroup.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  Enabled := False;

  OnClearFrameData := @DoClearFrameData;
  OnLoadFrameData := @DoLoadFrameData;
  OnSaveFrameData := @DoSaveFrameData;
end;

destructor TfmETKGUIactMergeGroup.Destroy;
begin
  inherited Destroy;
end;

end.

