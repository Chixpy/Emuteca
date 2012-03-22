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

{ Unit of Progress Bar form. }
unit fProgress;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, Buttons;

type

  { TfrmProgress }

  TfrmProgress = class(TForm)
    bCancel: TBitBtn;
    lInfo1: TLabel;
    lAction: TLabel;
    lInfo2: TLabel;
    ProgressBar: TProgressBar;
    procedure bCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  protected
    Continue: Boolean;
  public
    { public declarations }
    function UpdateProgressBar(const Value, MaxValue : Int64): Boolean;
    function UpdTextAndBar(const aAction, Info1, Info2: String;
      const Value, MaxValue: Int64): Boolean;
  end;

var
  frmProgress: TfrmProgress;

implementation

{ TfrmProgress }

procedure TfrmProgress.FormCreate(Sender: TObject);
begin
  Continue := True;
end;

procedure TfrmProgress.bCancelClick(Sender: TObject);
begin
  Continue := False;
end;

function TfrmProgress.UpdateProgressBar(const Value, MaxValue : Int64): Boolean;
begin
  Application.ProcessMessages;
  Result := Continue;

  if MaxValue = 0 then
  begin
    Self.Visible := False;
    Exit;
  end;

  if not Self.Visible then
    Self.Visible := True;

  ProgressBar.Max := MaxValue;
  ProgressBar.Position := Value;
end;

function TfrmProgress.UpdTextAndBar(const aAction, Info1, Info2: String;
  const Value, MaxValue: Int64): Boolean;
begin
  lAction.Caption:= aAction;
  lInfo1.Caption := Info1;
  lInfo2.Caption := Info2;
  Result := UpdateProgressBar(Value, MaxValue);
end;

initialization
  {$I fProgress.lrs}

end.

