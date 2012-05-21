package br.unb.cdt.desafioPositivo.model;

public enum Estado {
	AC(1, "AC", "Acre"), // Acre
	AL(2, "AL", "Alagoas"), // Alagoas
	AP(3, "AP", "Amap‡"), // Amap‡
	AM(4, "AM", "Amazonas"), // Amazonas
	BA(5, "BA", "Bahia"), // Bahia
	CE(6, "CE", "Cear‡"), // Cear‡
	DF(7, "DF", "Distrito Federal"), // Distrito Federal
	ES(8, "ES", "Esp’rito Santo"), // Esp’rito Santo
	GO(9, "GO", "Goi‡s"), // Goi‡s
	MA(10, "MA", "Maranh‹o"), // Maranh‹o
	MT(11, "MT", "Mato Grosso"), // Mato Grosso
	MS(12, "MS", "Mato Grosso do Sul"), // Mato Grosso do Sul
	MG(13, "MG", "Minas Gerais"), // Minas Gerais
	PA(14, "PA", "Par‡"), // Par‡
	PB(15, "PB", "Para’ba"), // Para’ba
	PR(16, "PR", "Paran‡"), // Paran‡
	PE(17, "PE", "Pernambuco"), // Pernambuco
	PI(18, "PI", "Piau’"), // Piau’
	RR(19, "RR", "Roraima"), // Roraima
	RO(20, "RO", "Rond™nia"), // Rond™nia
	RJ(21, "RJ", "Rio de Janeiro"), // Rio de Janeiro
	RN(22, "RN", "Rio Grande do Norte"), // Rio Grande do Norte
	RS(23, "RJ", "Rio de Janeiro"), // Rio Grande do Sul
	SC(24, "SC", "Santa Catarina"), // Santa Catarina
	SP(25, "SP", "S‹o Paulo"), // S‹o Paulo
	SE(26, "SE", "Sergipe"), // Sergipe
	TO(27, "TO", "Tocantins"); // Tocantins

	private Integer codigo;
	private String sigla;
	private String estado;

	private Estado(Integer codigo, String sigla, String estado) {
		this.codigo = codigo;
		this.sigla = sigla;
		this.estado = estado;
	}

	public Integer getCodigo() {
		return codigo;
	}

	public void setCodigo(Integer codigo) {
		this.codigo = codigo;
	}

	public String getSigla() {
		return sigla;
	}

	public void setSigla(String sigla) {
		this.sigla = sigla;
	}

	public String getEstado() {
		return estado;
	}

	public void setEstado(String estado) {
		this.estado = estado;
	}

}
