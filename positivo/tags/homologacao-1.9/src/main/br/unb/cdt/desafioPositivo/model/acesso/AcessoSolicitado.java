package br.unb.cdt.desafioPositivo.model.acesso;

import java.util.Calendar;

import javax.persistence.Column;
import javax.persistence.DiscriminatorValue;
import javax.persistence.Entity;
import javax.persistence.Table;

@Entity
@Table(name="TB_ACESSO_SOLICITADO")
@DiscriminatorValue("1")
public class AcessoSolicitado extends AcessoUsuario {

	private static final long serialVersionUID = 1L;

	@Column(name="CODIGO_EFETIVACAO")
	private String codigoEfetivacao;
	
	@Override
	public void autenticar(boolean autenticacao) throws ExcecaoAcessoUsuario {
		throw new ExcecaoAcessoUsuario("Nao e possivel autenticar o usuario. Necessario confirmar o cadastro");
	}

	@Override
	public void confirmarCadastro(String token) throws ExcecaoAcessoUsuario {
		if(codigoEfetivacao.equals(token)) {
			dataFim = Calendar.getInstance().getTime();
			AcessoUsuario acessoUsuario = new AcessoAtivo();
			acessoUsuario.setUsuario(usuario);
			usuario.getHistoricoSituacaoAcesso().add(acessoUsuario);
		}
		else {
			throw new ExcecaoAcessoUsuario("Codigo de confirmacao do cadastro nao confere");
		}
	}

	@Override
	public void desbloquear() throws ExcecaoAcessoUsuario {
		throw new ExcecaoAcessoUsuario("Nao e possivel desbloquear o acesso do usuario, que precisa confirmar o cadastro");
	}
	
	public String getCodigoEfetivacao() {
		return codigoEfetivacao;
	}

	public void setCodigoEfetivacao(String codigoEfetivacao) {
		this.codigoEfetivacao = codigoEfetivacao;
	}

	@Override
	public void alterarSenha() throws ExcecaoAcessoUsuario {
		throw new ExcecaoAcessoUsuario("Usuario com cadastro pendente.");	
	}

}
