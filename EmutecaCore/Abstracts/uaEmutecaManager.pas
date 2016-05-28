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
  uEmutecaCommon, uaEmutecaStorable;

type

  { caEmutecaManagerIni }

  caEmutecaManagerIni = class(caEmutecaStorableIni)
  private
    FProgressCallBack: TEmutecaProgressCallBack;
    procedure SetProgressCallBack(AValue: TEmutecaProgressCallBack);

  public
    property ProgressCallBack: TEmutecaProgressCallBack
      read FProgressCallBack write SetProgressCallBack;
    //< CallBack function to show the progress in actions.

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  end;

    { caEmutecaManagerTxt }

    caEmutecaManagerTxt = class(caEmutecaStorableTxt)
  private
    FProgressCallBack: TEmutecaProgressCallBack;
    procedure SetProgressCallBack(AValue: TEmutecaProgressCallBack);

  public
    property ProgressCallBack: TEmutecaProgressCallBack
      read FProgressCallBack write SetProgressCallBack;
    //< CallBack function to show the progress in actions.

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{ caEmutecaManagerTxt }

procedure caEmutecaManagerTxt.SetProgressCallBack(
  AValue: TEmutecaProgressCallBack);
begin
  if FProgressCallBack = AValue then
    Exit;
  FProgressCallBack := AValue;
end;

constructor caEmutecaManagerTxt.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
end;

destructor caEmutecaManagerTxt.Destroy;
begin
  inherited Destroy;
end;

{ caEmutecaManagerIni }

procedure caEmutecaManagerIni.SetProgressCallBack(
  AValue: TEmutecaProgressCallBack);
begin
  if FProgressCallBack = AValue then
    Exit;
  FProgressCallBack := AValue;
end;

constructor caEmutecaManagerIni.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
end;

destructor caEmutecaManagerIni.Destroy;
begin
  inherited Destroy;
end;


end.
