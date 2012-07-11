package br.unb.cdt.desafioPositivo.model;

public enum Categoria {
	JOGOS_ENTRETENIMENTO(1, "Jogos e entretenimento"),
	UTILITARIOS(2, "Utilitarios"),
	PRODUTIVIDADE(3, "Produtividade"),
	NOTICIAS(4, "Noticias"),
	MULTIMIDIA(5, "Multimidia"),
	EDUCATIVO(6, "Educativo"),
	REDES_SOCIAIS(7, "Redes sociais");
	
	private Integer codigo;
	private String descricao;
	
	private Categoria(Integer codigo, String descricao) {
		this.codigo = codigo;
		this.descricao = descricao;
	}

	public Integer getCodigo() {
		return codigo;
	}

	public String getDescricao() {
		return descricao;
	}
	
	
}
