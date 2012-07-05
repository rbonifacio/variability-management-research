package br.unb.cdt.desafioPositivo.util.rest;

/**
 * Enumeracao com os codigos de retorno de interesse 
 * do servico Positivo que permite cadastrar usuarios. 
 * 
 * @author rbonifacio
 */
public enum CodigoRespostaCadastro {
	SUCESSO(0),
	CLIENTE_JA_EXISTE(4),
	OUTROS(1);
	
	private Integer codigo;
	
	private CodigoRespostaCadastro(Integer codigo) {
		this.codigo = codigo;
	}

	public Integer getCodigo() {
		return codigo;
	}
	
	public static CodigoRespostaCadastro fromCodigo(Integer codigo) {
		for(CodigoRespostaCadastro c: values()) {
			if(codigo.equals(c.getCodigo())) {
				return c;
			}
		}
		return OUTROS;
	}
 	
}
