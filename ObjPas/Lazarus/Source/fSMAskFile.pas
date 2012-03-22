{ This file is part of Emuteca

  Copyright (C) 2006-2012 Chixpy

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

{ Unit with TfrmSMAskFile.

  Used by fScriptManager for ask to the user a filename.

  Although forms can be created by a script it provides a simple and easy
    interface.
}
unit fSMAskFile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  EditBtn, ExtCtrls, StdCtrls, Buttons;

type

  { TfrmSMAskFile }

  TfrmSMAskFile = class(TForm)
    bAccept: TBitBtn;
    bCancel: TBitBtn;
    eFileName: TFileNameEdit;
    lTitle: TLabel;
    pButtons: TPanel;
    pFilename: TPanel;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  frmSMAskFile: TfrmSMAskFile;

implementation

{ TfrmSMAskFile }

initialization
  {$I fSMAskFile.lrs}

end.

