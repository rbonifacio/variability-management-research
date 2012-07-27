package br.unb.cdt.desafioPositivo.util.rest;

/**
 * Enumeracao com os codigos de retorno do servico 
 * de autenticacao.
 * 
 * @author rbonifacio
 */
public enum CodigoRespostaNovaSenha {
	SUCESSO(0),
	SENHA_INVALIDA(2),
	CLIENTE_NAO_EXISTE(3),
	OUTROS(1);
	
	private Integer codigo;
	
	private CodigoRespostaNovaSenha(Integer codigo) {
		this.codigo = codigo;
	}

	public Integer getCodigo() {
		return codigo;
	}

	public void setCodigo(Integer codigo) {
		this.codigo = codigo;
	}
	
	public static CodigoRespostaNovaSenha fromCodigo(int codigo) {
		for(CodigoRespostaNovaSenha c : values()) {
			if (codigo == c.getCodigo()) {
				return c;
			}
		}
		return OUTROS;
	}
}
