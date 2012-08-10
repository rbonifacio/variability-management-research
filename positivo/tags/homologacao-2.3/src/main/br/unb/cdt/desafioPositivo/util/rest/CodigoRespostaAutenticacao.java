package br.unb.cdt.desafioPositivo.util.rest;

/**
 * Enumeracao com os codigos de retorno do servico 
 * de autenticacao.
 * 
 * @author rbonifacio
 */
public enum CodigoRespostaAutenticacao {
	SUCESSO(0),
	SERVICO_INDISPONIVEL(1),
	SENHA_INVALIDA(2),
	CLIENTE_NAO_ENCONTRADO(3),
	ID_APP_NAO_CONFERE(5),
	PARAMETROS_NULOS(7);
	
	private Integer codigo;
	
	private CodigoRespostaAutenticacao(Integer codigo) {
		this.codigo = codigo;
	}

	public Integer getCodigo() {
		return codigo;
	}

	public void setCodigo(Integer codigo) {
		this.codigo = codigo;
	}
	
	public static CodigoRespostaAutenticacao fromCodigo(int codigo) {
		for(CodigoRespostaAutenticacao c : values()) {
			if (codigo == c.getCodigo()) {
				return c;
			}
		}
		return null;
	}
}
