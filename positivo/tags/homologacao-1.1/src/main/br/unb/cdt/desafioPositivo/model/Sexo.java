package br.unb.cdt.desafioPositivo.model;

/**
 * Enumeracao com as definicoes do sexo dos 
 * proponentes. 
 * 
 * @author rbonifacio
 */
public enum Sexo {
	MASCULINO(1, "Masculino"),
	FEMININO(2, "Feminino");
	
	private Integer codigo;
	private String descricao;
	
	private Sexo(Integer codigo, String descricao) {
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
