{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Author: Dariusz Rorat

}

unit srcmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  Menus, ExtCtrls, StdCtrls, ExtDlgs, Buttons, ubarcodes;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnQuit: TBitBtn;
    btnLoadText: TBitBtn;
    cboCodeType: TComboBox;
    cboECCLevel: TComboBox;
    Label1: TLabel;
    InMemo: TMemo;
    Label2: TLabel;
    OpenDialog: TOpenDialog;
    SavePictureDialog: TSavePictureDialog;
    Shape1: TShape;
    Shape2: TShape;
    procedure btnLoadTextClick(Sender: TObject);
    procedure btnQuitClick(Sender: TObject);
    procedure cboCodeTypeEditingDone(Sender: TObject);
    procedure cboECCLevelEditingDone(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure InMemoChange(Sender: TObject);
  private
    QR: TBarcodeQR;
    Aztec: TBarcodeAztec;
    DataMatrix: TBarCodeDataMatrix;
    CurrentGenerator: TLazBarcodeCustomText;

    procedure SetupGenerator;
  public
    { public declarations }
  end; 

var
  frmMain: TfrmMain;

implementation

uses
  programconsts, gettext, translations;

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.SetupGenerator;
var
  intIndex: Integer;
  intEccLevel: Integer;
begin
  try
    intIndex    := cboCodeType.ItemIndex;
    intEccLevel := cboEccLevel.ItemIndex;
    if intEccLevel < 0 then
      intEccLevel := 0;
    case intIndex of
      0:
      begin
        CurrentGenerator := QR;
        QR.Visible := true;
        Aztec.Visible := false;
        Datamatrix.Visible := false;
        Label2.Visible := true;
        cboEcclevel.Visible := true;
        QR.ECCLevel := TBarcodeQR_ECCLevel(intEccLevel);
        QR.Text := InMemo.Text;
      end;
      1:
      begin
        CurrentGenerator := Aztec;
        QR.Visible := false;
        Aztec.Visible := true;
        Datamatrix.Visible := false;
        Label2.Visible := false;
        cboEcclevel.Visible := false;
        Aztec.Text := InMemo.Text;
      end;
      2:
      begin
        CurrentGenerator := DataMatrix;
        QR.Visible := false;
        Aztec.Visible := false;
        Datamatrix.Visible := true;
        Label2.Visible := false;
        cboEcclevel.Visible := false;
        Datamatrix.Text := InMemo.Text;
      end;
      else
      begin
        QR.Visible := false;
        Aztec.Visible := false;
        Label2.Visible := false;
        cboEcclevel.Visible := false;
        Datamatrix.Visible := false;
      end;
    end;

  except
    MessageDlg(strConvertError, mtError, [mbOK], 0);
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  PODirectory, Lang, FallbackLang: string;
begin
  PODirectory := '.\languages\';
  GetLanguageIDs(Lang, FallbackLang); // in unit gettext
  TranslateUnitResourceStrings('LCLStrConsts',
    PODirectory + 'lclstrconsts.%s.po', Lang, FallbackLang);
  TranslateUnitResourceStrings('ProgramConsts',
    PODirectory + 'messages.%s.po', Lang, FallbackLang);

  Caption := strCaption;
  Label1.Caption := strCodeType;
  Label2.Caption := strEccLevel;
  btnLoadText.Caption := strLoadFromText;
  btnQuit.Caption := strQuit;
  OpenDialog.Filter := strFilter;
  OpenDialog.Title := strOpenDialogTitle;

  QR := TBarcodeQR.Create(Self);
  QR.Parent := frmMain;
  QR.Left := Shape2.Left + 8;
  QR.Top := Shape2.Top + 8;
  QR.Width := Shape2.Width - 16;
  QR.Height := Shape2.Height - 16;
  QR.Visible := true;

  Aztec := TBarcodeAztec.Create(Self);
  Aztec.Parent := frmMain;
  Aztec.Left := Shape2.Left + 8;
  Aztec.Top := Shape2.Top + 8;
  Aztec.Width := Shape2.Width - 16;
  Aztec.Height := Shape2.Height - 16;
  Aztec.Visible := false;

  DataMatrix := TBarcodeDataMatrix.Create(Self);
  DataMatrix.Parent := frmMain;
  DataMatrix.Left := Shape2.Left + 8;
  DataMatrix.Top := Shape2.Top + 8;
  DataMatrix.Width := Shape2.Width - 16;
  DataMatrix.Height := Shape2.Height - 16;
  DataMatrix.Visible := false;
end;

procedure TfrmMain.btnQuitClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.cboCodeTypeEditingDone(Sender: TObject);
begin
  SetupGenerator;
end;

procedure TfrmMain.cboECCLevelEditingDone(Sender: TObject);
begin
  SetupGenerator;
end;

procedure TfrmMain.btnLoadTextClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    with TStringList.Create do
    begin
      LoadFromFile(UTF8ToSys(OpenDialog.FileName));
      InMemo.Text := SysToUTF8(Text);
      Free;
    end;
  end;
end;

procedure TfrmMain.InMemoChange(Sender: TObject);
begin
  SetupGenerator;
end;

end.
