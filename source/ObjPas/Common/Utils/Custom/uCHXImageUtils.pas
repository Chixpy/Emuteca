unit uCHXImageUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Menus, ActnList, Graphics,
  IniFiles, FileUtil, Buttons, ImgList,
  // Custom
  uCHXStrUtils;

procedure ReadActionsIcons(const aFileName, Section: string; BaseDir: string; ImageList: TImageList; ActionList: TCustomActionList);
{< Reads icons for the diferent actions a ImageList and assigns them.

  It reads a .ini file to search which images must be loaded, relative paths
    are searched from BaseDir.

  If ini file don't have the necesary key=value pair, then it will be created.

  @param(aFileName Filename of a ini file where the icons filenames are
    stored.)
  @param(Section Section where nfo will be searched.)
  @param(BaseDir Base directory where icons wth rlative path are searched from.)
  @param(ImageList An image list where images ade stored)
  @param(ActionList An action list which actions will be assigned an image.)
}

procedure ReadMenuIcons(const aFileName, Section: string; BaseDir: string; ImageList: TImageList; Menu: TMenu);
{< Reads icons for menu items with no action assigned and assigns them.

  It reads a .ini file to search which images must be loaded, relative paths
    are searched from BaseDir (or relative to ini file).

  If ini file don't have the necesary key=value pair, then it will be created.

  @param(aFileName Filename of a ini file where the icons filenames are
    stored.)
  @param(Section Section where nfo will be searched.)
  @param(BaseDir Base directory where icons wth rlative path are searched from.)
  @param(ImageList An image list where images ade stored)
  @param(Menu An menu which its items will be assigned an image.)
}

procedure FixComponentImagesFromActions(aComponent: TComponent);
{ Assings images from actions to components (and subcomponents).

  Some components (TBitButton, SpeedButton, ...) load their glyphs directly;
    and when a TAction is assigned, them don't load de corresponding image
    because they are not TImageList dependant.

  This procedure loads their glyphs from the image list assigned to their
    action.}

function AddToImageList(aImageList: TImageList;
  const FileName: String): integer;

function CorrectAspetRatio(OrigRect: TRect; aImage: TPicture): TRect;
{< Returns a TRect with the correct aspect ratio for the picture inside the
  OrigRect.
}

implementation

procedure ReadActionsIcons(const aFileName, Section: string; BaseDir: string; ImageList: TImageList; ActionList: TCustomActionList);
var
  IniFile: TMemIniFile;
  Cont: integer;
  IconFile: String;
begin
  BaseDir := SetAsFolder(BaseDir);
  if BaseDir = '' then
    BaseDir := ExtractFilePath(aFileName);
  ActionList.Images := ImageList;

  if Section = '' then Exit;

  IniFile := TMemIniFile.Create(UTF8ToSys(aFileName));
  try
    Cont := 0;
    while Cont < ActionList.ActionCount do
    begin
      IconFile := IniFile.ReadString(Section, ActionList.Actions[Cont].Name, '');
      if IconFile = '' then
      begin
        IconFile := ActionList.Actions[Cont].Name + '.png';
        IniFile.WriteString(Section, ActionList.Actions[Cont].Name, IconFile);
        IniFile.UpdateFile;
      end;
      TCustomAction(ActionList.Actions[Cont]).ImageIndex :=
        AddToImageList(ImageList, SetAsFolder(BaseDir) + IconFile);
      Inc(Cont);
    end;
  finally
    FreeAndNil(IniFile);
  end;
end;

procedure ReadMenuIcons(const aFileName, Section: string; BaseDir: string; ImageList: TImageList; Menu: TMenu);

  procedure ReadIcon(IniFile: TMemIniFile; ImageList: TImageList;
    Menu: TMenuItem; Section: String; BaseDir: String);
  var
    IconFile: string;
    Cont: integer;
  begin
    if not (Menu.IsLine or Assigned(Menu.Action)) then
    begin
      IconFile := IniFile.ReadString(Section, Menu.Name, '');
      if IconFile = '' then
      begin
        IconFile := Menu.Name + '.png';
        IniFile.WriteString(Section, Menu.Name, IconFile);
        IniFile.UpdateFile;
      end;
      Menu.ImageIndex := AddToImageList(ImageList, BaseDir + IconFile);
    end;

    Cont := 0;
    while Cont < Menu.Count do
    begin
      ReadIcon(IniFile, ImageList, Menu.Items[Cont], Section, BaseDir);
      Inc(Cont);
    end;
  end;

  //procedure ReadMenuIcons(const aFileName, Section, BaseDir: String;
  //  ImageList: TImageList; Menu: TMenu);
var
  IniFile: TMemIniFile;
  Cont: integer;
begin
  BaseDir := SetAsFolder(BaseDir);
  if BaseDir = '' then
    BaseDir := ExtractFilePath(aFileName);

  IniFile := TMemIniFile.Create(UTF8ToSys(aFileName));
  try
    Cont := 0;
    while Cont < Menu.Items.Count do
    begin
      ReadIcon(IniFile, ImageList, Menu.Items[Cont], Section,
        SetAsFolder(BaseDir));
      Inc(Cont);
    end;
  finally
    FreeAndNil(IniFile);
  end;
end;        

procedure FixComponentImagesFromActions(aComponent: TComponent);
  Procedure FixComponent(aComponent: TComponent);
  var
    ImageList: TCustomImageList;
    aAction: TCustomAction;
  begin
    if aComponent is TCustomBitBtn then
    begin
      with aComponent as TCustomBitBtn do
      begin
        if Assigned(Action) then
        begin
          // Not safe...
          aAction := TCustomAction(Action);
          ImageList := aAction.ActionList.Images;
          if (ImageList <> nil) and (aAction.Imageindex >= 0) then
            ImageList.GetBitmap(aAction.Imageindex, Glyph);
        end;
      end;
    end
    else
    begin
      if aComponent is TCustomSpeedButton then
      begin
        with aComponent as TCustomSpeedButton do
        begin
          if Assigned(Action) then
          begin
            // Not safe...
            aAction := TCustomAction(Action);
            ImageList := aAction.ActionList.Images;
            if (ImageList <> nil) and (aAction.Imageindex >= 0) then
              ImageList.GetBitmap(aAction.Imageindex, Glyph);
          end;
        end;
      end;
    end;
  end;
var
  i : integer;
begin
  i := 0;
  while i < aComponent.ComponentCount do
  begin
    FixComponent(aComponent.Components[i]);
    Inc(i);
  end;
end;

function AddToImageList(aImageList: TImageList;
  const FileName: String): integer;
var
  Image: TPicture;
  Extension: String;
begin
  Result := -1;
  if aImageList = nil then
    Exit;
  if FileExistsUTF8(FileName) then
  begin
    Image := TPicture.Create;
    try
      Image.LoadFromFile(FileName);
      // Cutrada para que los iconos se dibujen transparentes...
      Extension := ExtractFileExt(FileName);
      if (Extension = '.ico') or (Extension = '.icns') or
        (Extension = '.cur') then
        Result := aImageList.AddMasked(Image.Bitmap,
          Image.Icon.TransparentColor)
      else
        Result := aImageList.Add(Image.PNG, nil);
    finally
      FreeAndNil(Image);
    end;
  end;
end;

function CorrectAspetRatio(OrigRect: TRect; aImage: TPicture): TRect;
var
  Adjustment: integer;
begin
  Result := OrigRect;
  if aImage.Width > aImage.Height then
  begin
    // Crazy formula, don't ask
    Adjustment := Round(((OrigRect.Right - OrigRect.Left) *
      (1 - (aImage.Height / aImage.Width))) / 2);
    Result.Top := OrigRect.Top + Adjustment;
    Result.Bottom := OrigRect.Bottom - Adjustment;
  end
  else
  begin
    Adjustment := Round(((OrigRect.Bottom - OrigRect.Top) *
      (1 - (aImage.Width / aImage.Height))) / 2);
    Result.Left := OrigRect.Left + Adjustment;
    Result.Right := OrigRect.Right - Adjustment;
  end;
end;

end.

