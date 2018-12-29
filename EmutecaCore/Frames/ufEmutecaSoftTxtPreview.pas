unit ufEmutecaSoftTxtPreview;
{< TfmEmutecaSoftTxtPreview frame unit.

  This file is part of Emuteca Core.

  Copyright (C) 2018-2019 Chixpy

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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ActnList,
  // CHX units
  uCHXStrUtils,
  // CHX frames
  ufCHXTxtListPreview;

type

  { TfmEmutecaSoftTxtPreview }

  TfmEmutecaSoftTxtPreview = class(TfmCHXTxtListPreview)
    actSaveFile: TAction;
    actLockText: TAction;
    tbLockText: TToolButton;
    tbSaveFile: TToolButton;
    procedure actLockTextExecute(Sender: TObject);
    procedure actSaveFileExecute(Sender: TObject);

  private
    FSaveTextFolder: string;
    procedure SetSaveTextFolder(const aSaveTextFolder: string);

    protected
          procedure DoClearFrameData; override;

  public
    property SaveTextFolder: string read FSaveTextFolder write SetSaveTextFolder;

        constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

procedure TfmEmutecaSoftTxtPreview.actLockTextExecute(Sender: TObject);
begin
  mText.ReadOnly := actLockText.Checked;
end;

procedure TfmEmutecaSoftTxtPreview.actSaveFileExecute(Sender: TObject);
begin
  // TODO: Save text file.
end;

procedure TfmEmutecaSoftTxtPreview.SetSaveTextFolder(
  const aSaveTextFolder: string);
begin
  FSaveTextFolder := SetAsFolder(aSaveTextFolder);

  Enabled := SaveTextFolder <> '';
end;

procedure TfmEmutecaSoftTxtPreview.DoClearFrameData;
begin
  inherited DoClearFrameData;

  // Enabling buttons because a text can be added.
  Enabled := SaveTextFolder <> '';
end;

constructor TfmEmutecaSoftTxtPreview.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  actLockText.Checked := mText.ReadOnly;
end;

destructor TfmEmutecaSoftTxtPreview.Destroy;
begin
  inherited Destroy;
end;

end.

