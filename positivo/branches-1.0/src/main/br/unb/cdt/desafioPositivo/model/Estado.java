package br.unb.cdt.desafioPositivo.model;

public enum Estado {
	AC(1, "AC", "Acre"), // Acre
	AL(2, "AL", "Alagoas"), // Alagoas
	AM(3, "AM", "Amazonas"), // Amazonas
	AP(4, "AP", "Amapá"), // Amap�
	BA(5, "BA", "Bahia"), // Bahia
	CE(6, "CE", "Ceará"), // Cear�
	DF(7, "DF", "Distrito Federal"), // Distrito Federal
	ES(8, "ES", "Espírito Santo"), // Esp�rito Santo
	GO(9, "GO", "Goiás"), // Goi�s
	MA(10, "MA", "Maranhão"), // Maranh�o
	MG(11, "MG", "Minas Gerais"), // Minas Gerais
	MS(12, "MS", "Mato Grosso do Sul"), // Mato Grosso do Sul
	MT(13, "MT", "Mato Grosso"), // Mato Grosso
	PA(14, "PA", "Pará"), // Par�
	PB(15, "PB", "Paraíba"), // Para�ba
	PE(16, "PE", "Pernambuco"), // Pernambuco
	PI(17, "PI", "Piauí"), // Piau�
	PR(18, "PR", "Paraná"), // Paran�
	RO(19, "RO", "Rondônia"), // Rond�nia
	RR(20, "RR", "Roraima"), // Roraima
	RJ(21, "RJ", "Rio de Janeiro"), // Rio de Janeiro
	RN(22, "RN", "Rio Grande do Norte"), // Rio Grande do Norte
	RS(23, "RS", "Rio Grande do Sul"), // Rio Grande do Sul
	SC(24, "SC", "Santa Catarina"), // Santa Catarina
	SE(25, "SE", "Sergipe"), // Sergipe
	SP(26, "SP", "São Paulo"), // S�o Paulo
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
