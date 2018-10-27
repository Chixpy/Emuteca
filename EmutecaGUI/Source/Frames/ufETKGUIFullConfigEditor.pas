unit ufETKGUIFullConfigEditor;

{< TfmETKGUIFullConfigEditor frame unit.

  This file is part of Emuteca GUI.

  Copyright (C) 2018-2018 Chixpy

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
  // CHX frames
  ufCHXPropEditor,
  // CHX forms
  ufrCHXForm,
  // Emuteca Core units
  uEmutecaConst,
  // Emuteca GUI units
  uETKGUIConst, uETKGUIRscStr;

type

  { TfmETKGUIFullConfigEditor }

  TfmETKGUIFullConfigEditor = class(TfmCHXPropEditor)
  private

  public

    class function SimpleForm(aGUIIconsIni: string;
      aGUIConfigIni: string): integer;
    //< Creates a form with Emuteca GUI Config.

  end;

implementation

{$R *.lfm}

{ TfmETKGUIFullConfigEditor }

class function TfmETKGUIFullConfigEditor.SimpleForm(aGUIIconsIni: string;
  aGUIConfigIni: string): integer;
var
  aForm: TfrmCHXForm;
  aFrame: TfmETKGUIFullConfigEditor;
begin
  Result := mrNone;

  Application.CreateForm(TfrmCHXForm, aForm);
  try
    aForm.Name := krsETKGUIConfigID;
    aForm.Caption := Format(krsFmtWindowCaption,
      [Application.Title, rsETKGUIConfigTitle]);

    aFrame := TfmETKGUIFullConfigEditor.Create(aForm);
    aFrame.SaveButtons := True;
    aFrame.ButtonClose := True;
    aFrame.Align := alClient;


    aForm.LoadGUIConfig(aGUIConfigIni);
    aForm.LoadGUIIcons(aGUIIconsIni);
    aFrame.Parent := aForm;

    Result := aForm.ShowModal;
  finally
    aForm.Free;
  end;
end;

end.
