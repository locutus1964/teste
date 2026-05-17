"""
Copia as três versões de Analise_Financeira.xlsx para F:\\temp\\planilhas
e exibe qual é a mais atualizada (data do sistema + metadados internos do Excel).

Uso:
    python copiar_e_comparar.py
"""

import os
import shutil
from datetime import datetime

try:
    import openpyxl
except ImportError:
    print("Instale a dependência: pip install openpyxl")
    import sys; sys.exit(1)


DESTINO = r"F:\temp\planilhas"

ARQUIVOS = {
    "OneDrive_Documentos":  r"C:\Users\jairg\OneDrive\Documentos\Analise_Financeira.xlsx",
    "OneDrive_Excel":       r"C:\Users\jairg\OneDrive\Documentos\Excel\Analise_Financeira.xlsx",
    "Downloads_D":          r"D:\My Personal Files\Downloads\Analise_Financeira.xlsx",
}


def data_interna_excel(caminho):
    """Lê a data de última modificação gravada dentro do próprio arquivo xlsx."""
    try:
        wb = openpyxl.load_workbook(caminho, read_only=True, data_only=True)
        props = wb.properties
        modified = props.modified or props.created
        wb.close()
        if modified:
            # openpyxl retorna datetime com tzinfo; remove para exibição simples
            return modified.replace(tzinfo=None)
    except Exception:
        pass
    return None


def main():
    os.makedirs(DESTINO, exist_ok=True)
    print(f"Pasta de destino: {DESTINO}\n")

    resultados = []

    for nome, origem in ARQUIVOS.items():
        print(f"Processando: {nome}")

        if not os.path.exists(origem):
            print(f"  [AVISO] Arquivo não encontrado: {origem}\n")
            resultados.append({
                "nome": nome,
                "origem": origem,
                "destino": None,
                "data_sistema": None,
                "data_interna": None,
            })
            continue

        destino = os.path.join(DESTINO, f"{nome}.xlsx")
        shutil.copy2(origem, destino)          # copy2 preserva metadados de data

        data_sis = datetime.fromtimestamp(os.path.getmtime(origem))
        data_int = data_interna_excel(origem)

        print(f"  Copiado para : {destino}")
        print(f"  Data sistema : {data_sis.strftime('%d/%m/%Y %H:%M:%S')}")
        print(f"  Data interna : {data_int.strftime('%d/%m/%Y %H:%M:%S') if data_int else 'não disponível'}")
        print()

        resultados.append({
            "nome": nome,
            "origem": origem,
            "destino": destino,
            "data_sistema": data_sis,
            "data_interna": data_int,
        })

    # ── Determina o mais atualizado ──────────────────────────────────────────
    disponiveis = [r for r in resultados if r["data_sistema"] is not None]

    if not disponiveis:
        print("Nenhum arquivo encontrado. Verifique os caminhos.")
        return

    print("=" * 60)
    print("RESUMO — Ordenado do mais recente para o mais antigo")
    print("=" * 60)

    # Prefere data interna quando disponível (mais confiável que data do SO)
    def chave(r):
        return r["data_interna"] or r["data_sistema"]

    ordenados = sorted(disponiveis, key=chave, reverse=True)

    for i, r in enumerate(ordenados):
        data_int = r["data_interna"]
        data_sis = r["data_sistema"]
        prefixo = ">>> MAIS ATUALIZADO:" if i == 0 else f"    {i + 1}º lugar     :"
        data_ref = data_int or data_sis
        fonte = "data interna Excel" if data_int else "data do sistema de arquivos"
        print(f"{prefixo} {r['nome']}")
        print(f"     Data ({fonte}): {data_ref.strftime('%d/%m/%Y %H:%M:%S')}")
        print(f"     Caminho original: {r['origem']}")
        if i == 0 and data_int and data_sis and abs((data_int - data_sis).total_seconds()) > 60:
            print(f"     [OBS] Data do sistema difere da interna — pode ter sido copiado sem edição")
        print()

    mais_novo = ordenados[0]
    print(f"Arquivo mais atualizado: {mais_novo['destino']}")


if __name__ == "__main__":
    main()
