package br.unb.cdt.desafioPositivo.model.acesso;

import java.util.Calendar;

import javax.persistence.DiscriminatorValue;
import javax.persistence.Entity;
import javax.persistence.Table;

@Entity
@Table(name="TB_ACESSO_BLOQUEADO")
@DiscriminatorValue("3")
public class AcessoBloqueado extends AcessoUsuario {

	private static final long serialVersionUID = 1L;

	@Override
	public void autenticar(boolean autenticacao) throws ExcecaoAcessoUsuario {
		throw new ExcecaoAcessoUsuario("Usuario com acesso bloqueado.");
	}

	@Override
	public void confirmarCadastro(String token) throws ExcecaoAcessoUsuario {
		throw new ExcecaoAcessoUsuario("Usuario com acesso ativado.");
	}

	@Override
	public void desbloquear() throws ExcecaoAcessoUsuario {
		realizaTransicaoAcessoAtivo();
	}

	@Override
	public void alterarSenha() throws ExcecaoAcessoUsuario {
		realizaTransicaoAcessoAtivo();		
	}
	
	private void realizaTransicaoAcessoAtivo() {
		dataFim = Calendar.getInstance().getTime();
		AcessoUsuario acessoUsuario = new AcessoAtivo();
		acessoUsuario.setUsuario(usuario);
		usuario.getHistoricoSituacaoAcesso().add(acessoUsuario);
	}
}
