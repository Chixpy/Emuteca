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
  uEmutecaCommon, uaCHXStorable, ucEmutecaConfig;

type

  { caEmutecaManager }

  caEmutecaManager = class(caCHXStorable)
  private
    FConfig: cEmutecaConfig;
    FProgressCallBack: TEmutecaProgressCallBack;

  protected
    procedure SetConfig(AValue: cEmutecaConfig); virtual;
    procedure SetProgressCallBack(AValue: TEmutecaProgressCallBack); virtual;

  public
    property ProgressCallBack: TEmutecaProgressCallBack
      read FProgressCallBack write SetProgressCallBack;
    //< CallBack function to show the progress in actions.

    property Config: cEmutecaConfig read FConfig write SetConfig;

    procedure AssingAllTo(aList: TStrings); virtual; abstract;
    procedure AssingEnabledTo(aList: TStrings); virtual; abstract;

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation
{ caEmutecaManager }

procedure caEmutecaManager.SetConfig(AValue: cEmutecaConfig);
begin
  if FConfig = AValue then Exit;
  FConfig := AValue;
end;

procedure caEmutecaManager.SetProgressCallBack(AValue: TEmutecaProgressCallBack);
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
