unit ufEmutecaSystemITFEditor;
{< TfmEmutecaSystemITFEditor frame unit.

  This file is part of Emuteca Core.

  Copyright (C) 2011-2024 Chixpy
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, ActnList, StdCtrls,
  // CHX frames
  ufCHXPropEditor, ufCHXMultiFolderEditor,
  // EmutecaCore classes
  ucEmutecaSystem;

type

  { TfmEmutecaSystemITFEditor }

  TfmEmutecaSystemITFEditor = class(TfmCHXPropEditor)
    gbxImageFolders: TGroupBox;
    gbxTextFolders: TGroupBox;
    Splitter1: TSplitter;

  private
    FfmImageFolders: TfmCHXMultiFolderEditor;
    FSystem: cEmutecaSystem;
    FfmTextFolders: TfmCHXMultiFolderEditor;
    procedure SetSystem(AValue: cEmutecaSystem);

  protected
    property fmImageFolders: TfmCHXMultiFolderEditor read FfmImageFolders;
    property fmTextFolders: TfmCHXMultiFolderEditor read FfmTextFolders;

  public
    property System: cEmutecaSystem read FSystem write SetSystem;

    procedure LoadFrameData; override;
    procedure SaveFrameData; override;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmEmutecaSystemITFEditor }

procedure TfmEmutecaSystemITFEditor.SetSystem(AValue: cEmutecaSystem);
begin
  if FSystem = AValue then
    Exit;
  FSystem := AValue;

  if Assigned(System) then
  begin
    fmImageFolders.FolderList := System.ImageFolders;
    fmImageFolders.CaptionList := System.ImageCaptions;
    fmImageFolders.InitialFolder := System.BaseFolder;
    fmTextFolders.FolderList := System.TextFolders;
    fmTextFolders.CaptionList := System.TextCaptions;
    fmTextFolders.InitialFolder := System.BaseFolder;
  end
  else
  begin
    fmImageFolders.FolderList := nil;
    fmImageFolders.CaptionList := nil;
    fmTextFolders.FolderList := nil;
    fmTextFolders.CaptionList := nil;
  end;

  LoadFrameData;
end;

procedure TfmEmutecaSystemITFEditor.LoadFrameData;
begin
  inherited LoadFrameData;

  Enabled := assigned(System);

  if not Enabled then
  begin
    ClearFrameData;
    Exit;
  end;
end;

procedure TfmEmutecaSystemITFEditor.SaveFrameData;
begin
  inherited SaveFrameData;

  fmImageFolders.SaveFrameData;
  fmTextFolders.SaveFrameData;
end;

constructor TfmEmutecaSystemITFEditor.Create(TheOwner: TComponent);

  procedure CreateFrames;
  begin
    FfmImageFolders := TfmCHXMultiFolderEditor.Create(gbxImageFolders);
    fmImageFolders.SaveButtons := False;
    fmImageFolders.ButtonClose := False;
    fmImageFolders.Align := alClient;
    fmImageFolders.Parent := gbxImageFolders;

    FfmTextFolders := TfmCHXMultiFolderEditor.Create(gbxTextFolders);
    fmTextFolders.SaveButtons := False;
    fmTextFolders.ButtonClose := False;
    fmTextFolders.Align := alClient;
    fmTextFolders.Parent := gbxTextFolders;
  end;

begin
  inherited Create(TheOwner);

  CreateFrames;
end;

destructor TfmEmutecaSystemITFEditor.Destroy;
begin
  inherited Destroy;
end;

initialization
  RegisterClass(TfmEmutecaSystemITFEditor);

finalization
  UnRegisterClass(TfmEmutecaSystemITFEditor);
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
