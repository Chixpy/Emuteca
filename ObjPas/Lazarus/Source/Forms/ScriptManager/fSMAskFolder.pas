{ This file is part of Emuteca

  Copyright (C) 2006-2013 Chixpy

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

{ Unit with TfrmSMAskFolder.

  Used by ScriptManager for ask to the user a filename.

  Although forms can be created by a script it provides a simple and easy
    interface.
}
unit fSMAskFolder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, EditBtn, Buttons;

type

  { TfrmSMAskFolder }

  TfrmSMAskFolder = class(TForm)
    bAccept: TBitBtn;
    bCancel: TBitBtn;
    eDirectory: TDirectoryEdit;
    lTitle: TLabel;
    pButtons: TPanel;
    pFolderName: TPanel;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  frmSMAskFolder: TfrmSMAskFolder;

implementation

initialization
  {$I fSMAskFolder.lrs}

end.

