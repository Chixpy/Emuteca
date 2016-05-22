unit ugEmutecaPersList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl,
  uEmutecaCommon;

{ TODO : Implement Generic manager for emulators, systems, parents and software }


   { cEmutecaGenPersList }

 {  generic cEmutecaGenPersList<T> = class (specialize TFPGObjectList<T>)
  private
    FProgressCallBack: TEmutecaProgressCallBack;
    procedure SetProgressCallBack(AValue: TEmutecaProgressCallBack);

  public
    procedure LoadFromFile(aFile: string);
    procedure SaveToFile(aFile: string);

    property ProgressCallBack: TEmutecaProgressCallBack read FProgressCallBack write SetProgressCallBack;
    //< CallBack function to show the progress in actions.
  end;
}
implementation

end.

