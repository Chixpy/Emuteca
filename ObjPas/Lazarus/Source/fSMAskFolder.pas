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

