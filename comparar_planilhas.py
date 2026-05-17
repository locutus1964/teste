"""
Comparador de Planilhas Excel
Compara três versões do arquivo Analise_Financeira.xlsx e gera um relatório HTML.

Requisitos:
    pip install openpyxl pandas

Uso:
    python comparar_planilhas.py
"""

import sys
import os
from datetime import datetime

try:
    import pandas as pd
    import openpyxl
except ImportError:
    print("Instale as dependências: pip install openpyxl pandas")
    sys.exit(1)


# ── Configuração dos arquivos ────────────────────────────────────────────────

ARQUIVOS = {
    "OneDrive (Documentos)": r"C:\Users\jairg\OneDrive\Documentos\Analise_Financeira.xlsx",
    "OneDrive (Excel)":      r"C:\Users\jairg\OneDrive\Documentos\Excel\Analise_Financeira.xlsx",
    "Downloads (D:)":        r"D:\My Personal Files\Downloads\Analise_Financeira.xlsx",
}

SAIDA_HTML = "relatorio_comparacao.html"


# ── Funções utilitárias ──────────────────────────────────────────────────────

def carregar_planilha(rotulo, caminho):
    """Carrega todas as abas de um arquivo xlsx em um dict {aba: DataFrame}."""
    if not os.path.exists(caminho):
        print(f"  [AVISO] Arquivo não encontrado: {caminho}")
        return None
    try:
        wb = pd.ExcelFile(caminho, engine="openpyxl")
        abas = {}
        for aba in wb.sheet_names:
            df = wb.parse(aba, header=None, dtype=str)
            df = df.fillna("")
            abas[aba] = df
        print(f"  [OK] {rotulo} — {len(abas)} aba(s): {', '.join(wb.sheet_names)}")
        return abas
    except Exception as e:
        print(f"  [ERRO] {rotulo}: {e}")
        return None


def obter_metadados(caminho):
    """Retorna tamanho e data de modificação do arquivo."""
    if not os.path.exists(caminho):
        return "—", "—"
    tamanho = os.path.getsize(caminho)
    modificado = datetime.fromtimestamp(os.path.getmtime(caminho)).strftime("%d/%m/%Y %H:%M:%S")
    return f"{tamanho:,} bytes", modificado


def comparar_dataframes(df_a, df_b, label_a, label_b):
    """
    Compara dois DataFrames célula a célula.
    Retorna lista de dicts com as diferenças encontradas.
    """
    diferencas = []

    linhas = max(len(df_a), len(df_b))
    colunas = max(len(df_a.columns) if not df_a.empty else 0,
                  len(df_b.columns) if not df_b.empty else 0)

    for r in range(linhas):
        for c in range(colunas):
            val_a = ""
            val_b = ""
            try:
                val_a = df_a.iat[r, c] if r < len(df_a) and c < len(df_a.columns) else ""
            except Exception:
                pass
            try:
                val_b = df_b.iat[r, c] if r < len(df_b) and c < len(df_b.columns) else ""
            except Exception:
                pass

            if str(val_a).strip() != str(val_b).strip():
                col_letra = num_para_col(c + 1)
                diferencas.append({
                    "celula": f"{col_letra}{r + 1}",
                    label_a: val_a,
                    label_b: val_b,
                })

    return diferencas


def num_para_col(n):
    """Converte número de coluna para letra estilo Excel (1→A, 27→AA)."""
    resultado = ""
    while n > 0:
        n, resto = divmod(n - 1, 26)
        resultado = chr(65 + resto) + resultado
    return resultado


# ── Geração do relatório HTML ────────────────────────────────────────────────

CSS = """
body { font-family: Arial, sans-serif; font-size: 13px; margin: 20px; background: #f5f5f5; }
h1   { color: #2c3e50; }
h2   { color: #34495e; margin-top: 30px; border-bottom: 2px solid #bdc3c7; padding-bottom: 4px; }
h3   { color: #555; margin-top: 20px; }
table { border-collapse: collapse; width: 100%; margin-top: 8px; background: #fff; }
th   { background: #2c3e50; color: #fff; padding: 7px 10px; text-align: left; }
td   { padding: 6px 10px; border-bottom: 1px solid #eee; vertical-align: top; }
tr:hover td { background: #f0f4f8; }
.igual   { color: #27ae60; font-weight: bold; }
.diff    { color: #c0392b; font-weight: bold; }
.aviso   { color: #e67e22; }
.badge   { display:inline-block; padding:2px 8px; border-radius:12px; font-size:11px; }
.badge-ok   { background:#d5f5e3; color:#1e8449; }
.badge-diff { background:#fadbd8; color:#922b21; }
.badge-miss { background:#fdebd0; color:#784212; }
.resumo  { background:#fff; border:1px solid #ddd; padding:15px; border-radius:6px; margin-bottom:20px; }
.meta    { background:#eaf4fb; border:1px solid #aed6f1; padding:10px; border-radius:5px; margin-bottom:12px; }
"""


def html_tabela_diferencas(diferencas, col_a, col_b):
    if not diferencas:
        return '<p class="igual">Nenhuma diferença encontrada.</p>'
    linhas = "".join(
        f"<tr><td>{d['celula']}</td>"
        f"<td>{escape_html(str(d[col_a]))}</td>"
        f"<td>{escape_html(str(d[col_b]))}</td></tr>"
        for d in diferencas
    )
    return (
        f"<table><tr><th>Célula</th><th>{col_a}</th><th>{col_b}</th></tr>"
        f"{linhas}</table>"
    )


def escape_html(texto):
    return (texto
            .replace("&", "&amp;")
            .replace("<", "&lt;")
            .replace(">", "&gt;")
            .replace('"', "&quot;"))


def gerar_html(planilhas, arquivos):
    rotulos = list(planilhas.keys())
    pares = [(rotulos[i], rotulos[j])
             for i in range(len(rotulos))
             for j in range(i + 1, len(rotulos))]

    secoes = []

    # Metadados
    secoes.append("<h2>Metadados dos Arquivos</h2>")
    secoes.append('<div class="meta"><table>')
    secoes.append("<tr><th>Arquivo</th><th>Caminho</th><th>Tamanho</th><th>Última Modificação</th></tr>")
    for rotulo, caminho in arquivos.items():
        tam, mod = obter_metadados(caminho)
        disponivel = planilhas.get(rotulo) is not None
        badge = '<span class="badge badge-ok">OK</span>' if disponivel else '<span class="badge badge-miss">Não encontrado</span>'
        secoes.append(
            f"<tr><td>{badge} {rotulo}</td><td><code>{escape_html(caminho)}</code></td>"
            f"<td>{tam}</td><td>{mod}</td></tr>"
        )
    secoes.append("</table></div>")

    # Comparações par a par
    for rot_a, rot_b in pares:
        dados_a = planilhas.get(rot_a)
        dados_b = planilhas.get(rot_b)

        secoes.append(f"<h2>Comparação: <em>{rot_a}</em> vs <em>{rot_b}</em></h2>")

        if dados_a is None or dados_b is None:
            faltando = rot_a if dados_a is None else rot_b
            secoes.append(f'<p class="aviso">Arquivo "{faltando}" não disponível — comparação ignorada.</p>')
            continue

        abas_a = set(dados_a.keys())
        abas_b = set(dados_b.keys())
        todas_abas = sorted(abas_a | abas_b)

        total_diffs = 0

        for aba in todas_abas:
            secoes.append(f"<h3>Aba: {escape_html(aba)}</h3>")

            if aba not in abas_a:
                secoes.append(f'<p class="aviso">Aba ausente em "{rot_a}".</p>')
                continue
            if aba not in abas_b:
                secoes.append(f'<p class="aviso">Aba ausente em "{rot_b}".</p>')
                continue

            diffs = comparar_dataframes(dados_a[aba], dados_b[aba], rot_a, rot_b)
            total_diffs += len(diffs)

            if diffs:
                badge = f'<span class="badge badge-diff">{len(diffs)} diferença(s)</span>'
                secoes.append(badge)
                secoes.append(html_tabela_diferencas(diffs, rot_a, rot_b))
            else:
                secoes.append('<p class="igual">&#10003; Idênticas</p>')

        secoes.append(
            f'<p><strong>Total de diferenças nesta comparação: '
            f'<span class="{"diff" if total_diffs else "igual"}">{total_diffs}</span></strong></p>'
        )

    data_hora = datetime.now().strftime("%d/%m/%Y %H:%M:%S")
    html = f"""<!DOCTYPE html>
<html lang="pt-BR">
<head>
  <meta charset="UTF-8">
  <title>Comparação de Planilhas — Analise_Financeira</title>
  <style>{CSS}</style>
</head>
<body>
  <h1>Comparação de Planilhas — Analise_Financeira.xlsx</h1>
  <p>Gerado em: {data_hora}</p>
  {"".join(secoes)}
</body>
</html>"""
    return html


# ── Ponto de entrada ─────────────────────────────────────────────────────────

def main():
    print("=" * 60)
    print("Comparador de Planilhas Excel")
    print("=" * 60)

    planilhas = {}
    for rotulo, caminho in ARQUIVOS.items():
        print(f"\nCarregando: {rotulo}")
        planilhas[rotulo] = carregar_planilha(rotulo, caminho)

    disponiveis = sum(1 for v in planilhas.values() if v is not None)
    if disponiveis < 2:
        print("\n[ERRO] É necessário pelo menos dois arquivos acessíveis para comparar.")
        sys.exit(1)

    print(f"\nGerando relatório HTML → {SAIDA_HTML}")
    html = gerar_html(planilhas, ARQUIVOS)

    saida = os.path.join(os.path.dirname(os.path.abspath(__file__)), SAIDA_HTML)
    with open(saida, "w", encoding="utf-8") as f:
        f.write(html)

    print(f"Relatório salvo em: {saida}")
    print("\nAbra o arquivo no navegador para visualizar as diferenças.")


if __name__ == "__main__":
    main()
