{ This file is part of Emuteca

  Copyright (C) 2006-2016 Chixpy

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

{ Abstract managers unit. }
unit uaEmutecaManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uEmutecaCommon, uaCHXStorable;

type

  { caEmutecaManager }

  caEmutecaManager = class(caCHXStorable)
  private
    FProgressCallBack: TEmutecaProgressCallBack;
    procedure SetProgressCallBack(AValue: TEmutecaProgressCallBack);

  public
    property ProgressCallBack: TEmutecaProgressCallBack
      read FProgressCallBack write SetProgressCallBack;
    //< CallBack function to show the progress in actions.

    procedure AssingAllTo(aList: TStrings); virtual; abstract;
    procedure AssingEnabledTo(aList: TStrings); virtual; abstract;

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation
{ caEmutecaManager }

procedure caEmutecaManager.SetProgressCallBack(
  AValue: TEmutecaProgressCallBack);
begin
  if FProgressCallBack = AValue then
    Exit;
  FProgressCallBack := AValue;
end;

constructor caEmutecaManager.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
end;

destructor caEmutecaManager.Destroy;
begin
  inherited Destroy;
end;


end.
