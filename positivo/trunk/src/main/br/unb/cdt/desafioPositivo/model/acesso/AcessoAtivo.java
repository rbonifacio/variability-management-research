package br.unb.cdt.desafioPositivo.model.acesso;

import java.util.Calendar;
import java.util.Date;

import javax.persistence.DiscriminatorValue;
import javax.persistence.Entity;
import javax.persistence.Table;

@Entity
@Table(name="TB_ACESSO_ATIVO")
@DiscriminatorValue("2")
public class AcessoAtivo extends AcessoUsuario {
	protected Integer tentativasInvalidas;
	
	protected Date ultimoAcesso;
	@Override
	public void autenticar(boolean autenticacao) throws ExcecaoAcessoUsuario {
		if(autenticacao) {
			ultimoAcesso = Calendar.getInstance().getTime();
			tentativasInvalidas = 0;
		}
		else {
			tentativasInvalidas++;
			//TODO: bloquear ou nao o usuario? acho interessante!
		}
	}

	@Override
	public void confirmarCadastro(String token) throws ExcecaoAcessoUsuario {
		throw new ExcecaoAcessoUsuario("Usuario com acesso ativado.");
	}

}
