@echo off
setlocal
cd /d "%~dp0"

set QUARTO="C:\Program Files\RStudio\resources\app\bin\quarto\bin\quarto.exe"
set RBIN=C:\Program Files\R\R-4.6.1\bin
set PATH=%RBIN%;%PATH%
set QUARTO_R=%RBIN%\Rscript.exe

echo ===============================================
echo  Renderizando relatorio_final.qmd (HTML + PDF)
echo ===============================================

echo.
echo [1/2] Gerando HTML...
set TENTATIVAS=0
:tentar_html
set /a TENTATIVAS+=1
%QUARTO% render relatorio_final.qmd --to html
if %ERRORLEVEL% EQU 0 goto html_ok
if %TENTATIVAS% GEQ 6 goto html_falhou
echo    (arquivo ocupado, tentando novamente em alguns segundos...)
timeout /t 6 /nobreak >nul
goto tentar_html
:html_falhou
echo    [ERRO] Nao consegui gerar o HTML. Feche o PDF/HTML se estiver aberto em outro programa e tente de novo.
goto fim
:html_ok
echo    OK.

echo.
echo [2/2] Gerando PDF...
set TENTATIVAS=0
:tentar_pdf
set /a TENTATIVAS+=1
%QUARTO% render relatorio_final.qmd --to pdf
if %ERRORLEVEL% EQU 0 goto pdf_ok
if %TENTATIVAS% GEQ 6 goto pdf_falhou
echo    (arquivo ocupado, provavelmente sincronizando com o iCloud -- tentando de novo...)
timeout /t 6 /nobreak >nul
goto tentar_pdf
:pdf_falhou
echo    [ERRO] Nao consegui gerar o PDF. Feche o arquivo relatorio_final.pdf se estiver aberto (Foxit, Edge etc.) e tente de novo.
goto fim
:pdf_ok
echo    OK.

echo.
echo Limpando arquivos temporarios...
if exist relatorio_final.aux del /q relatorio_final.aux
if exist relatorio_final.log del /q relatorio_final.log
if exist relatorio_final.toc del /q relatorio_final.toc
if exist relatorio_final.knit.md del /q relatorio_final.knit.md

echo.
echo ===============================================
echo  Pronto! relatorio_final.html e relatorio_final.pdf atualizados.
echo ===============================================

:fim
echo.
pause
