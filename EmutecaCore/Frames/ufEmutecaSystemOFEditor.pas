unit ufEmutecaSystemOFEditor;
{< TfmEmutecaSystemOFEditor frame unit.

  This file is part of Emuteca Core.

  Copyright (C) 2024 Chixpy
}
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  // CHX frames
  ufCHXPropEditor, ufCHXMultiFolderEditor,
  // EmutecaCore classes
  ucEmutecaSystem;

type

  { TfmEmutecaSystemOFEditor }

  TfmEmutecaSystemOFEditor = class(TfmCHXPropEditor)
    gbxOtherFolders : TGroupBox;
  private
    FfmOtherFolders : TfmCHXMultiFolderEditor;
    FSystem : cEmutecaSystem;
    procedure SetSystem(const aValue : cEmutecaSystem);

  protected
    property fmOtherFolders : TfmCHXMultiFolderEditor read FfmOtherFolders;

  public
    property System : cEmutecaSystem read FSystem write SetSystem;

    procedure LoadFrameData; override;
    procedure SaveFrameData; override;

    constructor Create(TheOwner : TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmEmutecaSystemOFEditor }

procedure TfmEmutecaSystemOFEditor.SetSystem(const aValue : cEmutecaSystem);
begin
   if FSystem = AValue then
    Exit;
  FSystem := AValue;

  if Assigned(System) then
  begin
    fmOtherFolders.FolderList := System.OtherFolders;
    fmOtherFolders.CaptionList := System.OtherFCapt;
    fmOtherFolders.InitialFolder := System.BaseFolder;
  end
  else
  begin
    fmOtherFolders.FolderList := nil;
    fmOtherFolders.CaptionList := nil;
  end;

  LoadFrameData;
end;

procedure TfmEmutecaSystemOFEditor.LoadFrameData;
begin
  inherited LoadFrameData;

  Enabled := assigned(System);

  if not Enabled then
  begin
    ClearFrameData;
    Exit;
  end;
end;

procedure TfmEmutecaSystemOFEditor.SaveFrameData;
begin
  inherited SaveFrameData;

  fmOtherFolders.SaveFrameData;
end;

constructor TfmEmutecaSystemOFEditor.Create(TheOwner : TComponent);

  procedure CreateFrames;
  begin
    FfmOtherFolders := TfmCHXMultiFolderEditor.Create(gbxOtherFolders);
    fmOtherFolders.SaveButtons := False;
    fmOtherFolders.ButtonClose := False;
    fmOtherFolders.Align := alClient;
    fmOtherFolders.Parent := gbxOtherFolders;
  end;

begin
  inherited Create(TheOwner);

  CreateFrames;
end;

destructor TfmEmutecaSystemOFEditor.Destroy;
begin
  inherited Destroy;
end;

initialization
  RegisterClass(TfmEmutecaSystemOFEditor);

finalization
  UnRegisterClass(TfmEmutecaSystemOFEditor);
end.
{
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
