package br.unb.cdt.desafioPositivo.util.rest;

public enum CodigoRespostaEnviaEmail {
	SUCESSO(0),
	SERVICO_INDISPONIVEL(1),
	CLIENTE_NAO_EXISTE(3),
	ID_APP_NAO_CONFERE(5),
	PARAMETROS_NULOS(7),
	OUTROS(2);

	private Integer codigo;
	
	private CodigoRespostaEnviaEmail(Integer codigo) {
		this.codigo = codigo;
	}

	public Integer getCodigo() {
		return codigo;
	}
	
	public void setCodigo(Integer codigo) {
		this.codigo = codigo;
	}
	
	public static CodigoRespostaEnviaEmail fromCodigo(int codigo) {
		for(CodigoRespostaEnviaEmail c : values()) {
			if (codigo == c.getCodigo()) {
				return c;
			}
		}
		return OUTROS;
	}
	
	
}
