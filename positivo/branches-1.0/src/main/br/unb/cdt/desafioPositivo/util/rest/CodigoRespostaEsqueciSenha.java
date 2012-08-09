package br.unb.cdt.desafioPositivo.util.rest;

/**
 * Enumeracao com os codigos de retorno do servico 
 * de recuperação de senha positivo.
 * 
 * @author rbonifacio
 */
public enum CodigoRespostaEsqueciSenha {
	SUCESSO(0),
	SERVICO_INDISPONÍVEL(1),
	CLIENTE_NAO_EXISTE(3),
	ID_APP_NAO_CONFERE(5),
	TICKET_INVALIDO(6),
	PARAMETROS_NULOS(7),
	OUTROS(2);
	
	private Integer codigo;
	
	private CodigoRespostaEsqueciSenha(Integer codigo) {
		this.codigo = codigo;
	}

	public Integer getCodigo() {
		return codigo;
	}

	public void setCodigo(Integer codigo) {
		this.codigo = codigo;
	}
	
	public static CodigoRespostaEsqueciSenha fromCodigo(int codigo) {
		for(CodigoRespostaEsqueciSenha c : values()) {
			if (codigo == c.getCodigo()) {
				return c;
			}
		}
		return OUTROS;
	}
}
